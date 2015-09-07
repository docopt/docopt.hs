{-# LANGUAGE FlexibleInstances #-}

import Control.Monad (unless)
import System.Exit
import System.Console.ANSI

import System.Console.Docopt
import System.Console.Docopt.Types
import System.Console.Docopt.ParseUtils
import System.Console.Docopt.UsageParse (pDocopt)
import System.Console.Docopt.OptParse (getArguments)

import           Data.Map (Map)
import qualified Data.Map as M

import Data.List.Split
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS


instance ToJSON ArgValue where
  toJSON x = case x of
    MultiValue vs -> toJSON $ reverse vs
    Value v       -> toJSON v
    NoValue       -> toJSON Null
    Counted n     -> toJSON n
    Present       -> toJSON True
    NotPresent    -> toJSON False

instance ToJSON (Map Option ArgValue) where
  toJSON argmap =
    let argmap' = M.mapKeys humanize argmap
    in  toJSON argmap'

coloredString :: Color -> String -> String
coloredString c str = setSGRCode [SetColor Foreground Dull c]
                    ++ str
                    ++ setSGRCode [Reset]

green, red, yellow, blue, magenta :: String -> String
green   = coloredString Green
red     = coloredString Red
yellow  = coloredString Yellow
blue    = coloredString Blue
magenta = coloredString Magenta

forEach :: Monad m => [a] -> (a -> m b) -> m ()
forEach xs = sequence_ . flip map xs


main :: IO ()
main = do
  testFile <- readFile "test/testcases.docopt"
  --putStrLn rawTests
  let notCommentLine x = null x || ('#' /= head x)
      testFileClean = unlines $ filter notCommentLine $ lines testFile
      caseGroups = filter (not . null) $ splitOn "r\"\"\"" testFileClean
  --putStrLn $ head caseGroups
  forEach caseGroups $ \caseGroup -> do
    let [usage, rawCases] = splitOn "\"\"\"" caseGroup
        cases = filter (/= "\n") $ splitOn "$ " rawCases
    optFormat <- case runParser pDocopt M.empty "Usage" usage of
      Left e -> do
        putStrLn "couldn't parse usage text"
        return (Sequence [], M.empty)
      Right o -> return o
    putStrLn $ "Docopt:\n" ++ blue usage
    putStrLn $ "Pattern:\n" ++ magenta (show optFormat)
    --putStrLn $ "Cases:  " ++ show cases
    forEach cases $ \testcase -> do
      let (cmdline, rawTarget_) = break (== '\n') testcase
          rawTarget = filter (/= '\n') rawTarget_
          maybeTargetJSON = decode (BS.pack rawTarget) :: Maybe Value
          rawArgs = tail $ words cmdline
      parsedArgs <- case getArguments optFormat rawArgs of
        Left e -> do
          putStrLn $ "Parse Error: " ++ red (show e)
          return M.empty
        Right a -> return a
      let parsedArgsJSON = toJSON parsedArgs
          testCaseSuccess = if rawTarget == "\"user-error\""
            then M.null parsedArgs
            else maybeTargetJSON == Just parsedArgsJSON
      putStrLn $ "Case Cmd: " ++ yellow cmdline
      putStrLn $ "Case Target: " ++ (if testCaseSuccess then green else magenta) rawTarget
      unless testCaseSuccess $
        putStrLn $ "Failure: " ++ red (BS.unpack $ encode parsedArgsJSON)
      putStrLn ""
  exitSuccess
