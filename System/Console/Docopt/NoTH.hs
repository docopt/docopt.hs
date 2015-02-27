module System.Console.Docopt.NoTH
  (
    module System.Console.Docopt.NoTH,
    module System.Console.Docopt.Public
  )
  where

import Data.Map as M hiding (null)
import System.Exit

import System.Console.Docopt.Types
import System.Console.Docopt.Public
import System.Console.Docopt.ParseUtils
import System.Console.Docopt.UsageParse (pDocopt)
import System.Console.Docopt.OptParse (getArguments)


-- * Public API

-- ** Main option parsing entry points

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

optionsWithUsage :: String -> [String] -> IO Arguments
optionsWithUsage usage rawArgs =
    case runParser pDocopt M.empty "Usage" usage of
        Left _    -> do putStrLn usage
                        exitFailure
        Right fmt -> case getArguments fmt rawArgs of
            Left _           -> do putStrLn usage
                                   exitFailure
            Right parsedArgs -> return parsedArgs

optionsWithUsageDebug :: String -> [String] -> IO Arguments
optionsWithUsageDebug usage rawArgs =
    case runParser pDocopt M.empty "Usage" usage of
        Left err  -> fail $ show err
        Right fmt -> case getArguments fmt rawArgs of
            Left err         -> fail $ show err
            Right parsedArgs -> return parsedArgs
