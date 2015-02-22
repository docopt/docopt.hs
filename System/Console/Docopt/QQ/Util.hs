{-# OPTIONS_HADDOCK hide, prune #-}
module System.Console.Docopt.QQ.Util
    ( module System.Console.Docopt.QQ.Util
    , module System.Console.Docopt.Types
    , module System.Console.Docopt.ApplicativeParsec
    ) where

import System.Environment

import System.Console.Docopt.Types
import System.Console.Docopt.UsageParse
import System.Console.Docopt.OptParse

import qualified Data.Map as M

import System.Console.Docopt.ApplicativeParsec

-- | An abstract data type which represents Docopt usage patterns.
data Docopt = Docopt { optFormat :: OptFormat
                     -- | Retrieve the original usage string.
                     , usage :: String
                     }

parseFmt :: FilePath -> String -> Either ParseError OptFormat
parseFmt = runParser pDocopt M.empty

-- | Parse command line arguments.
parseArgs :: Docopt -> IO (Either ParseError Arguments)
parseArgs parser = getArguments (optFormat parser) <$> getArgs

-- | Same as 'parseArgs', but throw an error on failure.
parseArgs' :: Docopt -> IO Arguments
parseArgs' parser = either (error . show) id <$> parseArgs parser
