module System.Console.Docopt.OptParse 
  where

import qualified Data.Map as M
import           Data.List (intercalate, nub)

import System.Console.Docopt.ParseUtils
import System.Console.Docopt.Types


-- | The meat and potatoes.
buildOptParser :: String ->
                  -- ^ an obscure delimiter with which to intercalate the args list
                  OptFormat -> 
                  -- ^ the expected form of the options 
                  CharParser OptParserState ()
                  -- ^ a CharParser with which a ParsedArguments (k,v) list can be built
buildOptParser delim fmt@(pattern, infomap) = case pattern of
  (Sequence pats) ->
        foldl (andThen) (return ()) ps 
      where inner_dops = (\pat -> (pat, infomap)) `map` pats
            ps = (buildOptParser delim) `map` inner_dops
            andThen = \p1 p2 -> do 
              p1
              do st <- getState 
                 if not $ inShortOptStack st
                   then optional (try (string delim <?> "argument word break"))
                   else return ()
              p2
  (OneOf pats) ->
        choice $ makeParser `map` inner_dops
      where inner_dops = (\pat -> (pat, infomap)) `map` pats
            makeParser = \opts -> try $ buildOptParser delim opts
  (Optional pat) ->
        optional $ try (buildOptParser delim (pat, infomap)) 
  (Repeated pat) ->
        do many1 $ buildOptParser delim (pat, infomap) >> optional (try (string delim))
           return ()
  (Atom pat) -> case pat of 
      o@(ShortOption c) ->
            do st <- getState
               if inShortOptStack st then return () else char '-' >> return ()
               char c
               updateState $ updateInShortOptStack True
               val <- if expectsVal $ M.findWithDefault (fromSynList []) o infomap 
                 then try $ do 
                   (optional (char '=')) <|> (optional (string delim))
                   val <- many (notFollowedBy (string delim) >> anyChar)
                   updateState $ updateInShortOptStack False
                   return val
                 else return ""
               updateState $ withEachSynonym o $
                             \pa syn -> saveOccurrence syn val pa
      o@(LongOption name) ->
            do (string "--" >> string name)
               val <- if expectsVal $ M.findWithDefault (fromSynList []) o infomap 
                 then try $ do 
                   optional $ string "=" <|> string delim
                   many (notFollowedBy (string delim) >> anyChar)
                 else return ""
               updateState $ withEachSynonym o $
                           \pa syn -> saveOccurrence syn val pa
               updateState $ updateInShortOptStack False
          <?> "--"++name
      o@(AnyOption) ->
            let synlists = nub . map synonyms $ M.elems infomap
                parseOneOf syns = choice $ (\pat -> try $ buildOptParser delim (Atom pat, infomap)) `map` syns
                synparsers = parseOneOf `map` synlists
            in (foldl (<|>) (fail "any option")) . (map try) $ synparsers
      o@(Argument name) ->
            do val <- try $ many1 (notFollowedBy (string delim) >> anyChar)
               updateState $ updateParsedArgs $ saveOccurrence o val
               updateState $ updateInShortOptStack False
      o@(Command name) ->
            do string name
               updateState $ updateParsedArgs $ assertPresent o
               updateState $ updateInShortOptStack False

-- ** Helpers



-- | converts a parser to return its user-state
--   instead of its return value
returnState :: CharParser u a -> CharParser u u
returnState p = p >> getState

updateInShortOptStack :: Bool -> OptParserState -> OptParserState
updateInShortOptStack b ops = ops {inShortOptStack = b}

updateParsedArgs :: (Arguments -> Arguments) -> OptParserState -> OptParserState
updateParsedArgs f st = st {parsedArgs = f $ parsedArgs st}

--saveOccurrence :: Option -> String -> Arguments -> Arguments
--saveOccurrence o val argmap = M.insertWith f o [val] argmap
--    where f (newval:[]) oldval@("":vs) = newval : vs
--          f  newval     oldval         = newval ++ oldval
saveOccurrence :: Option -> String -> Arguments -> Arguments
saveOccurrence opt newval argmap = M.adjust updateVal opt argmap
    where updateVal oldval = case oldval of
              (MultiValue vs) -> MultiValue $ newval : vs
              (Value v)       -> Value newval
              NoValue         -> Value newval
              (Counted n)     -> Counted (n+1)
              Present         -> Present
              NotPresent      -> Present

--assertPresent :: Option -> Arguments -> Arguments
--assertPresent o argmap = M.insertWith f o [""] argmap
--    where f newval oldval = oldval 
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
          Just val -> Just $ MultiValue $ words val
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
        initialArgVals = foldl f M.empty patAtoms
            where f argmap atom = M.alter (\_ -> optInitialValue (infomap M.! atom) atom) atom argmap
        initialState = (fromOptInfoMap infomap) {parsedArgs = initialArgVals}
    in runParser p initialState "arguments" (delim `intercalate` rawargs)

