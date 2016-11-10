-- | Example:
--
-- @
-- {-\# LANGUAGE QuasiQuotes \#-}
-- module Main where
--
-- import Control.Monad (when)
-- import Data.Char (toUpper)
-- import System.Environment (getArgs)
-- import System.Console.Docopt
--
-- patterns :: Docopt
-- patterns = [docopt|
-- docopt-sample version 0.1.0
--
-- Usage:
--   docopt-sample cat \<file\>
--   docopt-sample echo [--caps] \<string\>
--
-- Options:
--   -c, --caps    Caps-lock the echoed argument
-- |]
--
-- getArgOrExit = getArgOrExitWith patterns
--
-- main :: IO ()
-- main = do
--   args <- parseArgsOrExit patterns =<< getArgs
--
--   when (args \`isPresent\` (command \"cat\")) $ do
--     file <- args \`getArgOrExit\` (argument \"file\")
--     putStr =<< readFile file
--
--   when (args \`isPresent\` (command \"echo\")) $ do
--     let charTransform = if args \`isPresent\` (longOption \"caps\")
--                         then toUpper
--                         else id
--     string <- args \`getArgOrExit\` (argument \"string\")
--     putStrLn $ map charTransform string
-- @
module System.Console.Docopt
  (
    module System.Console.Docopt.QQ,
    module System.Console.Docopt.Public
  )
  where

import System.Console.Docopt.QQ
import System.Console.Docopt.Public
