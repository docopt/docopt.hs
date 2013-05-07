module System.Console.Docopt.UsageParse 
  where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.List (nub)

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

flatSequence = flatten . Sequence
flatOneOf = flatten . OneOf


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
saveOptionsExpectVal t pairs = do
    updateState $ \st -> foldl save st pairs
    where save infomap (name, optExpectsVal) = M.alter alterFn opt infomap
            where opt = t name
                  alterFn oldval = Just $ case oldval of
                    Just oldinfo -> oldinfo {expectsVal = optExpectsVal || (expectsVal oldinfo)}
                    Nothing -> (fromSynList [opt]) {expectsVal = optExpectsVal}


pShortOption :: CharParser OptInfoMap (Char, Bool)
pShortOption = try $ do char '-' 
                        ch <- letter 
                        expectsVal <- pOptionArgument
                        return (ch, expectsVal)

pStackedShortOption :: CharParser OptInfoMap OptPattern
pStackedShortOption = try $ do 
    char '-'
    chars <- many1 $ letter
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
    (try $ char '=') <|> try inlineSpace
    notFollowedBy (char '-')
    try pArgument <|> try (many1 $ oneOf alphanumerics)
    return True

pArgument :: CharParser OptInfoMap String
pArgument = (try bracketStyle) <|> (try upperStyle) 
            where bracketStyle = do
                      open <- char '<' 
                      name <- many $ oneOf alphanumSpecial
                      close <- char '>'
                      return $ [open]++name++[close]
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
    tryRepeat <- ((try $ (optional inlineSpace) >> ellipsis) >> (return Repeated)) <|> (return id)
    return (tryRepeat expct)

pExp :: CharParser OptInfoMap OptPattern
pExp = inlineSpaces >> repeatable value
     where value = flatOneOf <$> pReqGroup
               -- <|> Optional . flatten . OneOf <$> betweenS "[(" ")]" pLine
               <|> flatten . Sequence . (map Optional) <$> try (betweenS "[" "]" pExp)
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
        usageLines <- (pUsageLine `sepEndBy` endline)
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
                      expectsVal = or $ map snd pairs
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
    maybeDefault <- optionMaybe pDefaultTag
    return maybeDefault

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
    let optPattern' = expectSynonyms optInfoMap optPattern
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
    e@(AnyOption)      -> flatten $ Unordered $ nub $ map Atom $ concat $ map synonyms (M.elems oim)
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
    (OneOf ps)     -> foldl (||) False $ map ((flip canRepeat) target) ps
    (Unordered ps) -> canRepeatInside ps || (atomicOccurrences ps > 1)
    (Optional p)   -> canRepeat p target
    (Repeated p)   -> target `elem` (atoms pat)
    (Atom a)       -> False
  where canRepeatInside ps = foldl (||) False $ map ((flip canRepeat) target) ps      
        atomicOccurrences ps = length $ filter (== target) $ atoms $ Sequence ps

