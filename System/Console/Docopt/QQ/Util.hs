{-# OPTIONS_HADDOCK hide, prune #-}
module System.Console.Docopt.QQ.Util
  (
    module System.Console.Docopt.QQ.Util
  )
  where

import System.Exit

import System.Console.Docopt.Types
import System.Console.Docopt.UsageParse
import System.Console.Docopt.OptParse

import qualified Data.Map as M

import System.Console.Docopt.ApplicativeParsec

parseFmt :: FilePath -> String -> Either ParseError OptFormat
parseFmt = runParser pDocopt M.empty

-- | Parse command line arguments.
parseArgs :: Docopt -> [String] -> Either ParseError Arguments
parseArgs parser argv = getArguments (optFormat parser) argv

-- | Same as 'parseArgs', but 'exitWithUsage' on parse failure.
parseArgsOrExit :: Docopt -> [String] -> IO Arguments
parseArgsOrExit parser argv = either (const $ exitWithUsage parser) return $ parseArgs parser argv

-- | Exit with help message.
exitWithUsage :: Docopt -> IO a
exitWithUsage doc = do
  putStr $ usage doc
  exitFailure
