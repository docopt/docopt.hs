{-# LANGUAGE FlexibleInstances #-}

import Control.Monad ( (>=>) )
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

import Test.HUnit
import Paths_docopt (getDataFileName)


instance ToJSON ArgValue where
  toJSON x = case x of
    MultiValue vs -> toJSON $ reverse vs
    Value v       -> toJSON v
    NoValue       -> toJSON Null
    Counted n     -> toJSON n
    Present       -> toJSON True
    NotPresent    -> toJSON False

instance {-# OVERLAPPING #-} ToJSON (Map Option ArgValue) where
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


main :: IO ()
main = do
  referenceTestsFile <- (getDataFileName >=> readFile) "test/testcases.docopt"
  referenceTests <- testsFromDocoptSpecFile "testcases.docopt" referenceTestsFile testcasesBlacklist

  regressionTestsFile <- (getDataFileName >=> readFile) "test/regressions.txt"
  regressionTests <- testsFromDocoptSpecFile "regressions.txt" regressionTestsFile regressionsBlacklist

  counts <- runTestTT $ TestList $ referenceTests ++ regressionTests
  exitWith $ if failures counts > 0
                then ExitFailure 1
                else ExitSuccess


testcasesBlacklist :: (Int, Int) -> Bool
-- Short/long option synonym equality (will fix)
testcasesBlacklist (4, 1) = True
testcasesBlacklist (4, 3) = True
testcasesBlacklist (7, 1) = True
testcasesBlacklist (8, 1) = True
testcasesBlacklist (8, 2) = True
testcasesBlacklist (35, 1) = True
testcasesBlacklist (64, 1) = True
-- Partial-option disambiguation
testcasesBlacklist (4, 2) = True
testcasesBlacklist (6, 3) = True
testcasesBlacklist (6, 4) = True
testcasesBlacklist (12, 4) = True
-- Stacked short options/flags disambiguation
testcasesBlacklist (14, 1) = True
testcasesBlacklist (70, 1) = True
-- Option order insensitivity
testcasesBlacklist (15, 2) = True
testcasesBlacklist (16, 2) = True
testcasesBlacklist (17, 2) = True
testcasesBlacklist (18, 2) = True
-- Weirdly broken (argument capture; should fix)
testcasesBlacklist (33, 2) = True
testcasesBlacklist (33, 3) = True
testcasesBlacklist (34, 3) = True
-- [options] expansion pruning (should fix)
testcasesBlacklist (67, 1) = True
testcasesBlacklist _ = False

regressionsBlacklist :: (Int, Int) -> Bool
-- Failing tests for issue #25, should investigate & fix
regressionsBlacklist (6, 1) = True
regressionsBlacklist (6, 2) = True
regressionsBlacklist _ = False


testsFromDocoptSpecFile :: String
                        -> String
                        -> ((Int, Int) -> Bool)
                        -> IO [Test]
testsFromDocoptSpecFile name testFile ignore =
  let notCommentLine x = null x || ('#' /= head x)
      testFileClean = unlines $ filter notCommentLine $ lines testFile
      caseGroups = filter (not . null) $ splitOn "r\"\"\"" testFileClean

  in
  return . (:[]) . TestLabel name . test $ zip caseGroups [(1 :: Int)..] >>= \(caseGroup, icg) -> do

    let [usage, rawCases] = splitOn "\"\"\"" caseGroup
        cases = filter (/= "\n") $ splitOn "$ " rawCases

    let (optFormat, docParseMsg) = case runParser pDocopt M.empty "Usage" usage of
          Left e -> ((Sequence [], M.empty), "Couldn't parse usage text")
          Right o -> (o, "")

    let groupDescLines = [
            docParseMsg,
            "Docopt:",
            blue usage,
            "Pattern:",
            magenta (show optFormat)
          ]

    zip cases [(1::Int)..] >>= \(testcase, itc) -> do

      let (cmdline, rawTarget_) = break (== '\n') testcase
          rawTarget = filter (/= '\n') rawTarget_
          maybeTargetJSON = decode (BS.pack rawTarget) :: Maybe Value
          rawArgs = tail $ words cmdline

      let (parsedArgs, argParseMsg) = case getArguments optFormat rawArgs of
            Left e -> (M.empty, "Parse Error: " ++ red (show e) ++ "\n")
            Right a -> (a, "")

      let parsedArgsJSON = toJSON parsedArgs
          testCaseEquality = if rawTarget == "\"user-error\""
            then M.null parsedArgs
            else maybeTargetJSON == Just parsedArgsJSON
          blacklisted = ignore (icg, itc)
          testCaseSuccess = if blacklisted
            then not testCaseEquality
            else testCaseEquality

      let testDescLines = [
              "Cmd: " ++ yellow cmdline,
              "Target: " ++ (if testCaseSuccess then green else magenta) rawTarget
            ]

      let testMsg = unlines . filter (not . null) $
                      groupDescLines
                      ++ testDescLines
                      ++ ["(Blacklisted)" | blacklisted]
                      ++ ["Failure: " ++ red (BS.unpack $ encode parsedArgsJSON)]

      let ti = TestCase $ testCaseSuccess @? testMsg

      return $ TestLabel ("group-" ++ show icg ++ "-case-" ++ show itc) ti
