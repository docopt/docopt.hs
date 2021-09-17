module System.Console.Docopt.UsageParse
  where

import qualified Data.Map as M
import           Data.Ord (comparing)
import           GHC.Exts (Down(..))
import           Data.List (nub, sortBy, maximumBy, dropWhileEnd)

import System.Console.Docopt.ParseUtils
import System.Console.Docopt.Types

-- * Helpers

-- | Flattens the top level of a Pattern, as long as that
--   /does not/ alter the matching semantics of the Pattern
flatten :: Pattern a -> Pattern a
flatten (Sequence (x:[]))  = x
flatten (OneOf (x:[]))     = x
flatten (Unordered (x:[])) = x
flatten x                  = x

flatSequence :: [Pattern a] -> Pattern a
flatSequence = flatten . Sequence

flatOneOf :: [Pattern a] -> Pattern a
flatOneOf = flatten . OneOf

trimEmptyLines :: String -> String
trimEmptyLines s = trimmed s ++ "\n"
  where
    isNewline = (== '\n')
    trimmed = dropWhile isNewline . dropWhileEnd isNewline


-- * Pattern Parsers

pLine :: CharParser OptInfoMap OptPattern
pLine = flatten . OneOf <$> pSeq `sepBy1` (inlineSpaces >> pipe)
        where pSeq = Sequence <$> (pExp `sepEndBy` inlineSpaces1)

pExpSeq :: CharParser OptInfoMap OptPattern
pExpSeq = flatten . Sequence <$> (pExp `sepEndBy1` inlineSpaces1)

pOptGroup :: CharParser OptInfoMap [OptPattern]
pOptGroup = pGroup '[' pExpSeq ']'

pReqGroup :: CharParser OptInfoMap [OptPattern]
pReqGroup = pGroup '(' pExpSeq ')'

saveOptionsExpectVal :: (a -> Option) -> [(a, Bool)] -> CharParser OptInfoMap ()
saveOptionsExpectVal t pairs = updateState $ \st -> foldl save st pairs
    where save infomap (name, optExpectsVal) = M.alter alterFn opt infomap
            where opt = t name
                  alterFn oldval = Just $ case oldval of
                    Just oldinfo -> oldinfo {expectsVal = optExpectsVal || expectsVal oldinfo}
                    Nothing -> (fromSynList [opt]) {expectsVal = optExpectsVal}


pShortOption :: CharParser OptInfoMap (Char, Bool)
pShortOption = try $ do char '-'
                        ch <- letter
                        expectsVal <- pOptionArgument
                        return (ch, expectsVal)

pStackedShortOption :: CharParser OptInfoMap OptPattern
pStackedShortOption = try $ do
    char '-'
    chars <- many1 letter
    lastExpectsVal <- pOptionArgument
    let (firstChars, lastChar) = (init chars, last chars)
        firstPairs = map (\x -> (x,False)) firstChars
        lastPair = (lastChar, lastExpectsVal)
    saveOptionsExpectVal ShortOption (firstPairs ++ [lastPair])
    case length chars of
      0 -> fail ""
      1 -> return $ Atom . ShortOption $ head chars
      _ -> return $ Unordered $ map (Atom . ShortOption) chars

pLongOption :: CharParser OptInfoMap (Name, Bool)
pLongOption = try $ do
    string "--"
    name <- many1 $ oneOf alphanumerics
    expectsVal <- pOptionArgument
    --let expectsVal = False
    return (name, expectsVal)

pAnyOption :: CharParser OptInfoMap String
pAnyOption = try (string "options")

pOptionArgument :: CharParser OptInfoMap Bool -- True if one is encountered, else False
pOptionArgument = option False $ try $ do
    try (char '=') <|> try inlineSpace
    notFollowedBy (char '-')
    try pArgument <|> try (many1 $ oneOf alphanumerics)
    return True

pArgument :: CharParser OptInfoMap String
pArgument = try bracketStyle <|> try upperStyle
            where bracketStyle = do
                      open <- char '<'
                      name <- many $ oneOf alphanumSpecial
                      close <- char '>'
                      return name
                  upperStyle = do
                      first <- oneOf uppers
                      rest <- many $ oneOf $ uppers ++ numerics
                      return $ first:rest

pCommand :: CharParser OptInfoMap String
pCommand = many1 (oneOf alphanumerics)

-- '<arg>...' make an OptPattern Repeated if followed by ellipsis
repeatable :: CharParser OptInfoMap OptPattern -> CharParser OptInfoMap OptPattern
repeatable p = do
    expct <- p
    tryRepeat <- (try (optional inlineSpace >> ellipsis) >> return Repeated) <|> return id
    return (tryRepeat expct)

pExp :: CharParser OptInfoMap OptPattern
pExp = inlineSpaces >> repeatable value
     where value = flatOneOf <$> pReqGroup
               -- <|> Optional . flatten . OneOf <$> betweenS "[(" ")]" pLine
               <|> flatten . Sequence . map Optional <$> try (betweenS "[" "]" pExp)
               <|> Optional . flatten . OneOf <$> pOptGroup
               <|> return (Atom AnyOption) <* pAnyOption
               <|> pStackedShortOption
               <|> do (name, expectsVal) <- pLongOption
                      saveOptionsExpectVal LongOption [(name, expectsVal)]
                      return $ Atom $ LongOption name
               <|> Atom . Argument <$> pArgument
               <|> Atom . Command <$> pCommand


-- * Usage Pattern Parsers

pUsageHeader :: CharParser OptInfoMap String
pUsageHeader = caseInsensitive "Usage:"

-- | Ignores leading spaces and first word, then parses
--   the rest of the usage line
pUsageLine :: CharParser OptInfoMap OptPattern
pUsageLine =
    try $ do
        inlineSpaces
        many1 (satisfy (not . isSpace)) -- prog name
        pLine

pUsagePatterns :: CharParser OptInfoMap OptPattern
pUsagePatterns = do
        many (notFollowedBy pUsageHeader >> anyChar)
        pUsageHeader
        optionalEndline
        usageLines <- pUsageLine `sepEndBy` endline
        return $ flatten . OneOf $ usageLines

-- * Option Synonyms & Defaults Parsers

-- | Succeeds only on the first line of an option explanation
--   (one whose first non-space character is '-')
begOptionLine :: CharParser OptInfoMap String
begOptionLine = inlineSpaces >> lookAhead (char '-') >> return "-"

pOptSynonyms :: CharParser OptInfoMap ([Option], Bool)
pOptSynonyms = do inlineSpaces
                  pairs <- p `sepEndBy1` (optional (char ',') >> inlineSpace)
                  let options = map fst pairs
                      expectsVal = any snd pairs
                  return (options, expectsVal)
               where p =   (\(c, ev) -> (ShortOption c, ev)) <$> pShortOption
                       <|> (\(s, ev) -> (LongOption s, ev)) <$> pLongOption

pDefaultTag :: CharParser OptInfoMap String
pDefaultTag = do
    caseInsensitive "[default:"
    inlineSpaces
    def <- many (noneOf "]")
    char ']'
    return def

pOptDefault :: CharParser OptInfoMap (Maybe String)
pOptDefault = do
    skipUntil (pDefaultTag <|> (newline >> begOptionLine))
    optionMaybe pDefaultTag

pOptDescription :: CharParser OptInfoMap ()
pOptDescription = try $ do
    (syns, expectsVal) <- pOptSynonyms
    def <- pOptDefault
    skipUntil (newline >> begOptionLine)
    updateState $ \infomap ->
      let optinfo = (fromSynList syns) {defaultVal = def, expectsVal = expectsVal}
          saveOptInfo infomap expct = M.insert expct optinfo infomap
      in  foldl saveOptInfo infomap syns
    return ()

pOptDescriptions :: CharParser OptInfoMap OptInfoMap
pOptDescriptions = do
    skipUntil (newline >> begOptionLine)
    optional newline
    optional $ pOptDescription `sepEndBy` endline
    getState


-- | Main usage parser: parses all of the usage lines into an Exception,
--   and all of the option descriptions along with any accompanying
--   defaults, and returns both in a tuple
pDocopt :: CharParser OptInfoMap OptFormat
pDocopt = do
    optPattern <- pUsagePatterns
    optInfoMap <- pOptDescriptions
    let optPattern' = eagerSort $ expectSynonyms optInfoMap optPattern
        saveCanRepeat pat el minfo = case minfo of
          (Just info) -> Just $ info {isRepeated = canRepeat pat el}
          (Nothing)   -> Just $ (fromSynList []) {isRepeated = canRepeat pat el}
        optInfoMap' = alterAllWithKey (saveCanRepeat optPattern') (atoms optPattern') optInfoMap
    return (optPattern', optInfoMap')


-- ** Pattern transformation & analysis

expectSynonyms :: OptInfoMap -> OptPattern -> OptPattern
expectSynonyms oim (Sequence exs)  = Sequence $ map (expectSynonyms oim) exs
expectSynonyms oim (OneOf exs)     = OneOf $ map (expectSynonyms oim) exs
expectSynonyms oim (Unordered exs) = Unordered $ map (expectSynonyms oim) exs
expectSynonyms oim (Optional ex)   = Optional $ expectSynonyms oim ex
expectSynonyms oim (Repeated ex)   = Repeated $ expectSynonyms oim ex
expectSynonyms oim a@(Atom atom)   = case atom of
    e@(Command ex)     -> a
    e@(Argument ex)    -> a
    e@AnyOption        -> flatten $ Unordered $ nub $ map Atom $ concatMap synonyms (M.elems oim)
    e@(LongOption ex)  ->
        case synonyms <$> e `M.lookup` oim of
          Just syns -> flatten . OneOf $ map Atom syns
          Nothing -> a
    e@(ShortOption c)  ->
        case synonyms <$> e `M.lookup` oim of
          Just syns -> flatten . OneOf $ map Atom syns
          Nothing -> a

canRepeat :: Eq a => Pattern a -> a -> Bool
canRepeat pat target =
  case pat of
    (Sequence ps)  -> canRepeatInside ps || (atomicOccurrences ps > 1)
    (OneOf ps)     -> canRepeatInside ps
    (Unordered ps) -> canRepeatInside ps || (atomicOccurrences ps > 1)
    (Optional p)   -> canRepeat p target
    (Repeated p)   -> target `elem` atoms pat
    (Atom a)       -> False
  where canRepeatInside = any (`canRepeat` target)
        atomicOccurrences ps = length $ filter (== target) $ atoms $ Sequence ps


-- | Compare on specificity of parsers built from optA and optB,
--   so we can be sure the parser tries the most-specific first, where possible.
--   E.g.
--     LongOption "option" > ShortOption 'o' == True
--     Command "cmd" > Argument "arg"        == True
compareOptSpecificity :: Option -> Option -> Ordering
compareOptSpecificity optA optB = case optA of
    LongOption a  -> case optB of
      LongOption b  -> comparingFirst length a b
      _             -> GT
    ShortOption a -> case optB of
      LongOption b  -> LT
      ShortOption b -> compare a b
      _             -> GT
    Command a     -> case optB of
      LongOption b  -> LT
      ShortOption b -> LT
      Command b     -> comparingFirst length a b
      _             -> GT
    Argument a    -> case optB of
      AnyOption     -> GT
      Argument b    -> comparingFirst length a b
      _             -> LT
    AnyOption     -> case optB of
      AnyOption     -> EQ
      _             -> LT
  where
    comparingFirst :: (Ord a, Ord b) => (a -> b) -> a -> a -> Ordering
    comparingFirst p a1 a2 =
      case compare (p a1) (p a2) of
        EQ -> compare a1 a2
        o  -> o

-- | Sort an OptPattern such that more-specific patterns come first,
--   while leaving the semantics of the pattern structure unchanged.
eagerSort :: OptPattern -> OptPattern
eagerSort pat = case pat of
    -- We special-case a top-level `OneOf` here because that's how
    -- the list of individual pattern lines are represented, and we
    -- never want to reorder those. This is inelegant, but effective
    -- enough for now.
    OneOf ps -> OneOf $ map innerSort ps
    a -> innerSort a
  where
    innerSort ipat = case ipat of
      Sequence ps  -> Sequence $ map innerSort ps
      OneOf ps     -> OneOf $   map innerSort
                              . sortBy (comparing $ Down . maxLength)
                              . sortBy (comparing representativeAtom)
                              $ ps
      Unordered ps -> Unordered $ map innerSort ps
      Optional p   -> Optional $ innerSort p
      Repeated p   -> Repeated $ innerSort p
      a@(Atom _)   -> a

    representativeAtom :: OptPattern -> Option
    representativeAtom p = case p of
      Sequence ps  -> if null ps then AnyOption else representativeAtom $ head ps
      OneOf ps     -> maximumBy compareOptSpecificity . map representativeAtom $ ps
      Unordered ps -> maximumBy compareOptSpecificity . map representativeAtom $ ps
      Optional p   -> representativeAtom p
      Repeated p   -> representativeAtom p
      Atom a       -> a

    maxLength :: OptPattern -> Int
    maxLength p = case p of
      Sequence ps  -> sum $ map maxLength ps
      OneOf ps     -> maximum $ map maxLength ps
      Unordered ps -> sum $ map maxLength ps
      Optional p   -> maxLength p
      Repeated p   -> maxLength p
      Atom a       -> case a of
        LongOption o  -> length o
        ShortOption _ -> 1
        Command c     -> length c
        Argument a    -> 100
        AnyOption     -> 0
