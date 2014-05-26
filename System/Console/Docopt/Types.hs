module System.Console.Docopt.Types
    where

import           Data.Ord (comparing)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.List (nub)

import System.Console.Docopt.ParseUtils

-- * Usage expression Types

type Name = String

data Pattern a = Sequence [Pattern a]
               | OneOf [Pattern a]
               | Unordered [Pattern a]
               | Optional (Pattern a)
               | Repeated (Pattern a)
               | Atom a
               deriving (Show, Eq)

atoms :: Eq a => Pattern a -> [a]
atoms (Sequence ps)  = foldl (++) [] $ map atoms ps
atoms (OneOf ps)     = foldl (++) [] $ map atoms $ nub ps
atoms (Unordered ps) = foldl (++) [] $ map atoms $ nub ps
atoms (Optional p)   = atoms p
atoms (Repeated p)   = atoms p
atoms (Atom a)       = [a]

data Option = LongOption Name
            | ShortOption Char
            | Command Name
            | Argument Name
            | AnyOption
            deriving (Show, Eq, Ord)

type OptPattern = Pattern Option

humanize :: Option -> String
humanize opt = case opt of
  Command name    -> name
  Argument name   -> name
  LongOption name -> "--"++name
  ShortOption c   -> ['-',c]
  AnyOption       -> "[options]"

-- | Used when parsing through the available option descriptions.
--   Holds a list of synonymous options, Maybe a default value (if specified),
--   an expectsVal :: Bool that indicates whether this option is a flag (--flag) 
--   or an option that needs an argument (--opt=arg), and isRepeated :: Bool 
--   that indicates whether this option is always single or needs to be accumulated
data OptionInfo = OptionInfo 
                  { synonyms :: [Option]
                  , defaultVal :: Maybe String
                  , expectsVal :: Bool 
                  , isRepeated :: Bool
                  } deriving (Show, Eq)

fromSynList :: [Option] -> OptionInfo
fromSynList opts = OptionInfo { synonyms = opts
                              , defaultVal = Nothing
                              , expectsVal = False
                              , isRepeated = False }

-- | Maps each available option to a OptionInfo entry
--   (each synonymous option gets its own separate entry, for easy lookup)
type OptInfoMap = Map Option OptionInfo

-- | Contains all the relevant information parsed out of a usage string.
--   Used to build the actual command-line arg parser.
type OptFormat = (OptPattern, OptInfoMap)

-- | 
data OptParserState = OptParserState 
                      { optInfoMap :: OptInfoMap
                      , parsedArgs :: Arguments
                      , inShortOptStack :: Bool
                      , inTopLevelSequence :: Bool
                      } deriving (Show)

fromOptInfoMap :: OptInfoMap -> OptParserState
fromOptInfoMap m = OptParserState { optInfoMap = m
                                  , parsedArgs = M.empty
                                  , inShortOptStack = False
                                  , inTopLevelSequence = True }


data ArgValue = MultiValue [String]
              | Value String
              | NoValue
              | Counted Int
              | Present
              | NotPresent
              deriving (Show, Eq, Ord)

-- | Maps each Option to all of the valued parsed from the command line
--   (in order of last to first, if multiple values encountered)
type Arguments = Map Option ArgValue

