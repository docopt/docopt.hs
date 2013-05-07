module System.Console.Docopt.OptParse 
  where

import Control.Monad (unless)

import qualified Data.Map as M
import           Data.List (intercalate, nub, (\\))

import System.Console.Docopt.ParseUtils
import System.Console.Docopt.Types


-- | The meat and potatoes.
buildOptParser :: String ->
                  -- ^ an obscure delimiter with which to intercalate the args list
                  OptFormat -> 
                  -- ^ the expected form of the options 
                  CharParser OptParserState ()
                  -- ^ a CharParser with which a ParsedArguments (k,v) list can be built
buildOptParser delim fmt@(pattern, infomap) = 
  let argDelim = (try $ string delim) <?> "space between arguments"
      makeParser p = buildOptParser delim (p, infomap)
      argDelimIfNotInShortOptStack = do 
        st <- getState 
        if not $ inShortOptStack st
          then optional argDelim
          else return ()
  in  case pattern of
  (Sequence pats) ->
      assertTopConsumesAll $ foldl (andThen) (return ()) ps 
      --foldl (andThen) (return ()) ps 
      where assertTopConsumesAll p = do
              st <- getState
              if inTopLevelSequence st
                then do 
                  updateState $ \st -> st {inTopLevelSequence = False}
                  p <* eof
                else p 
            inner_pats = (\pat -> (pat, infomap)) `map` pats
            ps = (buildOptParser delim) `map` inner_pats
            andThen = \p1 p2 -> do 
              p1
              argDelimIfNotInShortOptStack
              p2
  (OneOf pats) ->
      choice $ (try . makeParser) `map` pats
      --where inner_pats = (\pat -> (pat, infomap)) `map` pats
            --makeParser = \opts -> try $ buildOptParser delim opts
  (Unordered pats) ->
      choice $ (parseThisThenRest pats) `map` pats
      where parseThisThenRest list pat = try $ do
              makeParser pat
              let rest = list \\ [pat]
              argDelimIfNotInShortOptStack
              makeParser $ Unordered rest
  (Optional pat) ->
        case pat of 
          Unordered ps ->
            optional $ choice $ (parseThisThenRest ps) `map` ps
            where parseThisThenRest list pat = try $ do
                    makeParser pat
                    let rest = list \\ [pat]
                    argDelimIfNotInShortOptStack
                    makeParser $ Optional $ Unordered rest
          _ -> optional $ try $ makeParser pat 
  (Repeated pat) -> do
      case pat of 
        (Optional _) -> (try $ makeParser pat) `sepBy` argDelim
        _            -> (try $ makeParser pat) `sepBy1` argDelim
      return ()
  (Atom pat) -> case pat of 
      o@(ShortOption c) ->
            do  st <- getState
                if inShortOptStack st then return () else char '-' >> return ()
                char c
                updateState $ updateInShortOptStack True
                val <- if expectsVal $ M.findWithDefault (fromSynList []) o infomap 
                  then try $ do 
                    optional $ string "=" <|> argDelim
                    updateState $ updateInShortOptStack False
                    manyTill anyChar (lookAhead_ argDelim <|> eof)
                  else do
                    stillInShortStack <- isNotFollowedBy argDelim
                    unless stillInShortStack $ 
                      updateState $ updateInShortOptStack False 
                    return ""
                updateState $ withEachSynonym o $
                              \pa syn -> saveOccurrence syn val pa
          <?> humanize o
      o@(LongOption name) ->
            do string "--"
               string name
               val <- if expectsVal $ M.findWithDefault (fromSynList []) o infomap 
                 then do 
                   string "=" <|> argDelim
                   --many (notFollowedBy (string delim) >> anyChar)
                   manyTill anyChar (lookAhead_ argDelim <|> eof)
                 else return ""
               updateState $ withEachSynonym o $
                           \pa syn -> saveOccurrence syn val pa
               updateState $ updateInShortOptStack False
          <?> humanize o
      o@(AnyOption) ->
            let synlists = nub . map synonyms $ M.elems infomap
                --oneOf syns = OneOf (map Atom syns)
                --synparsers = oneOf `map` synlists
                oneOfSyns = map (\ss -> OneOf (map Atom ss)) synlists
                unorderedSynParser = buildOptParser delim (Unordered oneOfSyns, infomap)
            in  unorderedSynParser 
                <?> humanize o
      o@(Argument name) ->
            do val <- try $ many1 (notFollowedBy argDelim >> anyChar)
               updateState $ updateParsedArgs $ saveOccurrence o val
               updateState $ updateInShortOptStack False
          <?> humanize o
      o@(Command name) ->
            do string name
               updateState $ updateParsedArgs $ assertPresent o
               updateState $ updateInShortOptStack False
          <?> humanize o

-- ** Helpers



-- | converts a parser to return its user-state
--   instead of its return value
returnState :: CharParser u a -> CharParser u u
returnState p = p >> getState

updateInShortOptStack :: Bool -> OptParserState -> OptParserState
updateInShortOptStack b ops = ops {inShortOptStack = b}

updateParsedArgs :: (Arguments -> Arguments) -> OptParserState -> OptParserState
updateParsedArgs f st = st {parsedArgs = f $ parsedArgs st}

saveOccurrence :: Option -> String -> Arguments -> Arguments
saveOccurrence opt newval argmap = M.adjust updateVal opt argmap
    where updateVal oldval = case oldval of
              (MultiValue vs) -> MultiValue $ newval : vs
              (Value v)       -> Value newval
              NoValue         -> Value newval
              (Counted n)     -> Counted (n+1)
              Present         -> Present
              NotPresent      -> Present

assertPresent :: Option -> Arguments -> Arguments
assertPresent opt argmap = saveOccurrence opt "" argmap

withEachSynonym :: Option -> 
                   (Arguments -> Option -> Arguments) -> 
                   OptParserState -> 
                   OptParserState
withEachSynonym opt savefn st = let infomap = optInfoMap st
                                    args = parsedArgs st
                                    syns = synonyms $ M.findWithDefault (fromSynList []) opt infomap
                                in st {parsedArgs = foldl savefn args syns}


optInitialValue :: OptionInfo -> Option -> Maybe ArgValue
optInitialValue info opt = 
  case isRepeated info of 
    True  -> case opt of
      (Command name)  -> Just $ Counted 0
      (Argument name) -> Just $ MultiValue []
      (AnyOption)     -> Nothing
      _               -> case expectsVal info of 
        True  -> case defaultVal info of
          Just val -> Just $ MultiValue $ reverse $ words val
          Nothing  -> Just $ MultiValue []
        False -> Just $ Counted 0
    False -> case opt of
      (Command name)  -> Just NotPresent
      (Argument name) -> Just NoValue
      (AnyOption)     -> Nothing
      _               -> case expectsVal info of
        True  -> case defaultVal info of
          Just val -> Just $ Value val
          Nothing  -> Just NoValue
        False -> Just NotPresent


getArguments :: OptFormat -> [String] -> Either ParseError Arguments
getArguments fmt rawargs = 
    let (pattern, infomap) = fmt
        delim = "«»"
        p = parsedArgs <$> (returnState $ buildOptParser delim fmt)
        patAtoms = atoms pattern
        infoKeys = (\\ [AnyOption]) $ M.keys infomap
        allAtoms = nub $ patAtoms ++ infoKeys
        initialArgVals = foldl f M.empty allAtoms
            where f argmap atom = M.alter (\_ -> optInitialValue (infomap M.! atom) atom) atom argmap
        initialState = (fromOptInfoMap infomap) {parsedArgs = initialArgVals}
    in runParser p initialState "arguments" (delim `intercalate` rawargs)

