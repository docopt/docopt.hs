module System.Console.Docopt.OptParse 
  where

import qualified Data.Map as M
import           Data.List (intercalate, nub)

import System.Console.Docopt.ParseUtils
import System.Console.Docopt.Types

-- | The meat and potatoes.
buildOptParser :: String ->
                  -- ^ an obscure delimiter with which to intercalate the args list
                  Docopt -> 
                  -- ^ the expected form of the options 
                  CharParser OptParserState ()
                  -- ^ a CharParser with which a ParsedArguments (k,v) list can be built
buildOptParser delim dop@((Sequence exs),      syndef) = 
    foldl1 (andThen) ps 
  where inner_dops = (\ex -> (ex, syndef)) `map` exs
        ps = (buildOptParser delim) `map` inner_dops
        andThen = \p1 p2 -> do 
          p1
          do st <- getState 
             if not $ inShortOptStack st
               then optional (try (string delim <?> "argument word break"))
               else return ()
          p2
buildOptParser delim dop@((OneOf exs),         syndef) = 
    choice $ makeParser `map` inner_dops
  where inner_dops = (\ex -> (ex, syndef)) `map` exs
        makeParser = \opts -> try $ buildOptParser delim opts
buildOptParser delim dop@((Optional ex),       syndef) = 
    optional $ try (buildOptParser delim (ex, syndef)) 
buildOptParser delim dop@((Repeated ex),       syndef) = 
    do many1 $ buildOptParser delim (ex, syndef) >> optional (try (string delim))
       return ()
buildOptParser delim dop@(e@(ShortOption c),   syndef) = 
    do st <- getState
       if inShortOptStack st then return () else char '-' >> return ()
       char c
       updateState $ updateInShortOptStack True
       val <- if expectsVal $ M.findWithDefault (fromSynList []) e syndef 
         then try $ do 
           (optional (char '=')) <|> (optional (string delim))
           val <- many (notFollowedBy (string delim) >> anyChar)
           updateState $ updateInShortOptStack False
           return val
         else return ""
       updateState $ withEachSynonym e $
                     \pa syn -> saveOccurrence syn val pa
buildOptParser delim dop@(e@(LongOption name), syndef) = 
    do (string "--" >> string name)
       val <- if expectsVal $ M.findWithDefault (fromSynList []) e syndef 
         then try $ do 
           optional $ string "=" <|> string delim
           many (notFollowedBy (string delim) >> anyChar)
         else return ""
       updateState $ withEachSynonym e $
                   \pa syn -> saveOccurrence syn val pa
       updateState $ updateInShortOptStack False
  <?> "--"++name
buildOptParser delim dop@((AnyOption),         syndef) = 
    let synlists = nub . map synonyms $ M.elems syndef
        parseOneOf syns = choice $ (\ex -> try $ buildOptParser delim (ex, syndef)) `map` syns
        synparsers = parseOneOf `map` synlists
    in (foldl1 (<|>)) . (map try) $ synparsers
buildOptParser delim dop@(e@(Argument name),   syndef) = 
    do val <- try $ many1 (notFollowedBy (string delim) >> anyChar)
       updateState $ updateParsedArgs $ saveOccurrence e val
       updateState $ updateInShortOptStack False
buildOptParser delim dop@(e@(Command name),    syndef) = 
    do string name
       updateState $ updateParsedArgs $ assertPresent e
       updateState $ updateInShortOptStack False

-- ** Helpers



-- | converts a parser to return its user-state
--   instead of its return value
returnState :: CharParser u a -> CharParser u u
returnState p = p >> getState

updateInShortOptStack :: Bool -> OptParserState -> OptParserState
updateInShortOptStack b ops = ops {inShortOptStack = b}

updateParsedArgs :: (ParsedArguments -> ParsedArguments) -> OptParserState -> OptParserState
updateParsedArgs f st = st {parsedArgs = f $ parsedArgs st}

saveOccurrence :: Expectation -> String -> ParsedArguments -> ParsedArguments
saveOccurrence e val pa = M.insertWith f e [val] pa
    where f (newval:[]) oldval@("":vs) = newval : vs
          f  newval     oldval         = newval ++ oldval

assertPresent :: Expectation -> ParsedArguments -> ParsedArguments
assertPresent e pa = M.insertWith f e [""] pa
    where f newval oldval = oldval 

withEachSynonym :: Expectation -> 
                   (ParsedArguments -> Expectation -> ParsedArguments) -> 
                   OptParserState -> 
                   OptParserState
withEachSynonym ex savefn st = let syndef = synDefMap st
                                   args = parsedArgs st
                                   syns = synonyms $ M.findWithDefault (fromSynList []) ex syndef
                               in st {parsedArgs = foldl savefn args syns}


getOptions :: Docopt -> [String] -> Either ParseError Options
getOptions dop rawargs = let (expct, syndef) = dop
                             delim = "«»"
                             p = toOptions <$> (returnState $ buildOptParser delim dop)
                         in runParser p (fromSynDefMap syndef) "arguments" (delim `intercalate` rawargs)

