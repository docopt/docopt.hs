module System.Console.Docopt.NoTH
  (
    -- * Public API
      parseUsage
    , parseUsageOrExit

    , module System.Console.Docopt.Public
  )
  where

import Data.Map as M hiding (null)
import System.Exit

import System.Console.Docopt.Types
import System.Console.Docopt.Public
import System.Console.Docopt.ParseUtils
import System.Console.Docopt.UsageParse (pDocopt)


parseUsage :: String -> Either ParseError Docopt
parseUsage usg =
  case runParser pDocopt M.empty "Usage" usg of
    Left e       -> Left e
    Right optfmt -> Right (Docopt optfmt usg)

parseUsageOrExit :: String -> IO Docopt
parseUsageOrExit usg = exitUnless $ parseUsage usg
  where
    exit message = putStrLn message >> exitFailure
    exitUnless = either (const $ exit usg) return
