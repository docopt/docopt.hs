module System.Console.Docopt.Types
    where

import           Data.Map (Map)
import qualified Data.Map as M

import System.Console.Docopt.ParseUtils

-- * Usage expression Types

type Name = String

data Expectation = Sequence [Expectation]   -- firstarg secondarg thirdarg
                 | OneOf [Expectation]      -- --flag | --other
                 | Optional Expectation     -- [--flag]
                 | Repeated Expectation     -- args...
                 | ShortOption Char         -- -f
                 | LongOption Name          -- --flag
                 | AnyOption                -- [options]
                 | Argument Name            -- <name>
                 | Command Name             -- commandname
                 deriving (Show, Eq, Ord)

-- * Main Functions

--main :: IO ()
--main = do
--    contents <- getContents
--    putStr $ unlines $ map (show . parseDocopt) $ lines contents

--testDocopt :: IO ()
--testDocopt = do
--    contents <- readFile "test_usage.txt"
--    print $ runParser pDocopt M.empty "" contents


-- | Used when parsing through the available option descriptions.
--   Holds a list of synonymous options, Maybe a default value (if specified),
--   and a Bool that indicates whether this option is a flag (--flag) 
--   or an option that needs an argument (--opt=arg)
data SynonymDefault = SynonymDefault 
                      { synonyms :: [Expectation]
                      , defaultVal :: Maybe String
                      , expectsVal :: Bool 
                      } deriving (Show, Eq, Ord)

fromSynList :: [Expectation] -> SynonymDefault
fromSynList es = SynonymDefault es Nothing False

-- | Maps each available option to a SynonymDefault entry
--   (each synonymous option gets its own separate entry, for easy lookup)
type SynDefMap = Map Expectation SynonymDefault

-- | Contains all the relevant information parsed out of a usage string.
--   Used to build the actual command-line arg parser.
type Docopt = (Expectation, SynDefMap)

-- | 
data OptParserState = OptParserState { synDefMap :: SynDefMap
                                     , parsedArgs :: ParsedArguments
                                     , inShortOptStack :: Bool
                                     } deriving (Show)

fromSynDefMap :: SynDefMap -> OptParserState
fromSynDefMap m = OptParserState {synDefMap=m, parsedArgs=M.empty, inShortOptStack=False}

toOptions :: OptParserState -> Options
toOptions st = (synDefMap st, parsedArgs st)

-- | Maps each Expectation to all of the valued parsed from the command line
--   (in order of last to first, if multiple values encountered)
type ParsedArguments = Map Expectation [String]

-- | The SynDefMap and ParsedArguments are all that need to be kept around
--   and given to the user, for use with all the public lookup methods.
type Options = (SynDefMap, ParsedArguments)

-- | Controls the behaviour of the parsing
data ParsingOptions = ParsingOptions { showParseErrors :: Bool }

defaultParsingOptions :: ParsingOptions
defaultParsingOptions = ParsingOptions { showParseErrors = False }