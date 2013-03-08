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
                  CharParser Options Options
                  -- ^ a CharParser with which a ParsedArguments (k,v) list can be built
buildOptParser delim dop@((Sequence exs),      syndef) = foldl1 (andThen) ps 
                                                         where inner_dops = (\ex -> (ex, syndef)) `map` exs
                                                               ps = (buildOptParser delim) `map` inner_dops
                                                               andThen = \p1 p2 -> p1 >> string delim >> p2
buildOptParser delim dop@((OneOf exs),         syndef) = choice $ (buildOptParser delim) `map` inner_dops
                                                         where inner_dops = (\ex -> (ex, syndef)) `map` exs
buildOptParser delim dop@((Optional ex),       syndef) = do optional $ try (buildOptParser delim dop) 
                                                            getState
buildOptParser delim dop@((Repeated ex),       syndef) = do many1 $ buildOptParser delim dop
                                                            getState
buildOptParser delim dop@(e@(ShortOption c),   syndef) = do (char '-' >> char c)
                                                            optional (char '=') <|> optional (string delim)
                                                            val <- many (notFollowedBy (string delim) >> anyChar)
                                                            updateState $ withEachSynonym e $
                                                                          \pa syn -> saveOccurrence syn val pa
                                                            getState
buildOptParser delim dop@(e@(LongOption name), syndef) = do (string "--" >> string name)
                                                            optional (char '=') <|> optional (string delim)
                                                            val <- manyTill anyChar (try (string delim)) -- many (notFollowedBy (string delim) >> anyChar)
                                                            updateState $ withEachSynonym e $
                                                                          \pa syn -> saveOccurrence syn val pa
                                                            getState
buildOptParser delim dop@((AnyOption),         syndef) = let synlists = nub . map fst $ M.elems syndef
                                                             parseOneOf syns = choice $ (\ex -> buildOptParser delim (ex, syndef)) `map` syns
                                                             synparsers = parseOneOf `map` synlists
                                                         in (foldl1 (>>)) . (map try) $ synparsers
buildOptParser delim dop@(e@(Argument name),   syndef) = do val <- many1 (notFollowedBy (string delim) >> anyChar)
                                                            updateState $ updateParsedArgs $ saveOccurrence e val
                                                            getState
buildOptParser delim dop@(e@(Command name),    syndef) = do string name
                                                            updateState $ updateParsedArgs $ assertPresent e
                                                            getState

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
                                 syns = fst $ M.findWithDefault ([], Nothing) ex syndef
                             in (syndef, foldl savef pa syns)


getOptions :: Docopt -> [String] -> Either ParseError Options
getOptions dop rawargs = let (expct, syndef) = dop
                             delim = "«»"
                             p = buildOptParser delim dop
                         in runParser p (syndef, M.empty) "arguments" (delim `intercalate` rawargs)

