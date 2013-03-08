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


-- | A Map from key expectation (long or short option) to value
--   of a tuple whose first element is a list of synonymous expectations
--   (e.g. -h, --help) and whose second element is Maybe a default value
--   for those synonymous expectations
type OptSynonymsDefaults = Map Expectation ([Expectation], (Maybe String))

type Docopt = (Expectation, OptSynonymsDefaults)

type ParsedArguments = Map Expectation [String]
type Options = (OptSynonymsDefaults, ParsedArguments)

