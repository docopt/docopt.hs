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
                  CharParser Options ()
                  -- ^ a CharParser with which a ParsedArguments (k,v) list can be built
buildOptParser delim dop@((Sequence exs),      syndef) = foldl1 (andThen) ps 
                                                         where inner_dops = (\ex -> (ex, syndef)) `map` exs
                                                               ps = (buildOptParser delim) `map` inner_dops
                                                               andThen = \p1 p2 -> p1 >> optional (try (string delim)) >> p2
buildOptParser delim dop@((OneOf exs),         syndef) = choice $ makeParser `map` inner_dops
                                                         where inner_dops = (\ex -> (ex, syndef)) `map` exs
                                                               makeParser = \opts -> try $ buildOptParser delim opts
buildOptParser delim dop@((Optional ex),       syndef) = optional $ try (buildOptParser delim (ex, syndef)) 
buildOptParser delim dop@((Repeated ex),       syndef) = do many1 $ buildOptParser delim (ex, syndef) >> optional (try (string delim))
                                                            return ()
buildOptParser delim dop@(e@(ShortOption c),   syndef) = do (char '-' >> char c)
                                                            val <- if expectsVal $ M.findWithDefault (fromSynList []) e syndef 
                                                              then try $ do 
                                                                (optional (char '=')) <|> (optional (string delim))
                                                                many (notFollowedBy (string delim) >> anyChar)
                                                              else return ""
                                                            updateState $ withEachSynonym e $
                                                                          \pa syn -> saveOccurrence syn val pa
buildOptParser delim dop@(e@(LongOption name), syndef) = do (string "--" >> string name)
                                                            val <- if expectsVal $ M.findWithDefault (fromSynList []) e syndef 
                                                              then try $ do 
                                                                optional $ string "=" <|> string delim
                                                                many (notFollowedBy (string delim) >> anyChar)
                                                              else return ""
                                                            updateState $ withEachSynonym e $
                                                                          \pa syn -> saveOccurrence syn val pa
                                                         <?> "--"++name
buildOptParser delim dop@((AnyOption),         syndef) = let synlists = nub . map synonyms $ M.elems syndef
                                                             parseOneOf syns = choice $ (\ex -> try $ buildOptParser delim (ex, syndef)) `map` syns
                                                             synparsers = parseOneOf `map` synlists
                                                         in (foldl1 (<|>)) . (map try) $ synparsers
buildOptParser delim dop@(e@(Argument name),   syndef) = do val <- try $ many1 (notFollowedBy (string delim) >> anyChar)
                                                            updateState $ updateParsedArgs $ saveOccurrence e val
buildOptParser delim dop@(e@(Command name),    syndef) = do string name
                                                            updateState $ updateParsedArgs $ assertPresent e

-- | converts a parser to return its user-state
--   instead of its return value
returnState :: CharParser u a -> CharParser u u
returnState p = p >> getState

updateParsedArgs :: (ParsedArguments -> ParsedArguments) -> Options -> Options
updateParsedArgs f (syndef, pa) = (syndef, f pa)

saveOccurrence :: Expectation -> String -> ParsedArguments -> ParsedArguments
saveOccurrence e val pa = M.insertWith f e [val] pa
    where f (newval:[]) oldval@("":vs) = newval : vs
          f  newval     oldval         = newval ++ oldval

assertPresent :: Expectation -> ParsedArguments -> ParsedArguments
assertPresent e pa = M.insertWith f e [""] pa
    where f newval oldval = oldval 

withEachSynonym :: Expectation -> 
                   (ParsedArguments -> Expectation -> ParsedArguments) -> 
                   Options -> 
                   Options
withEachSynonym ex savef o = let (syndef, pa) = o
                                 syns = synonyms $ M.findWithDefault (fromSynList []) ex syndef
                             in (syndef, foldl savef pa syns)


getOptions :: Docopt -> [String] -> Either ParseError Options
getOptions dop rawargs = let (expct, syndef) = dop
                             delim = "«»"
                             p = returnState $ buildOptParser delim dop
                         in runParser p (syndef, M.empty) "arguments" (delim `intercalate` rawargs)

