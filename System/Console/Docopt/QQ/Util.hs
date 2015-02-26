{-# OPTIONS_HADDOCK hide, prune #-}
module System.Console.Docopt.QQ.Util
  (
    module System.Console.Docopt.QQ.Util
  )
  where

import System.Environment

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

-- | Same as 'parseArgs', but throw an error on failure.
parseArgsOrExit :: Docopt -> [String] -> IO Arguments
parseArgsOrExit parser argv = either (fail . show) return $ parseArgs parser argv
