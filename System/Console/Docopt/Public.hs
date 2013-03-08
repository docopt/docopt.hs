module System.Console.Docopt.Public 
  where

import System.Environment (getArgs)
import Data.Map as M

import System.Console.Docopt.ParseUtils
import System.Console.Docopt.Types
import System.Console.Docopt.UsageParse (pDocopt)
import System.Console.Docopt.OptParse (getOptions)


-- * Public API

-- ** Main option parsing entry points

optionsWithUsageFile :: FilePath -> IO Options
optionsWithUsageFile path = do usageStr <- readFile path
                               rawargs <- getArgs
                               case runParser pDocopt M.empty path usageStr of
                                   Left err -> fail usageStr
                                   Right dop -> case getOptions dop rawargs of
                                       Left err         -> fail usageStr
                                       Right parsedOpts -> return parsedOpts

optionsWithUsageFileDebug :: FilePath -> IO Options
optionsWithUsageFileDebug path = do usageStr <- readFile path
                                    rawargs <- getArgs
                                    case runParser pDocopt M.empty path usageStr of
                                        Left err  -> fail $ show err
                                        Right dop -> case getOptions dop rawargs of
                                            Left err         -> fail $ show err
                                            Right parsedOpts -> return parsedOpts

-- ** Option lookup methods

isPresent :: Options -> Expectation -> Bool
isPresent opts expct = let (_, pargs) = opts
                       in case expct `M.lookup` pargs of
                              Just _ -> True
                              Nothing -> False

notPresent :: Options -> Expectation -> Bool
notPresent o e = not $ isPresent o e

getArg :: Options -> Expectation -> Maybe String
getArg opts expct = let (syndef, pargs) = opts
                    in case expct `M.lookup` pargs of
                          Just vals -> Just $ head vals
                          Nothing -> (expct `M.lookup` syndef) >>= snd

getFirstArg :: Options -> Expectation -> Maybe String
getFirstArg opts expct = let (syndef, pargs) = opts
                         in case expct `M.lookup` pargs of
                              Just vals -> Just $ last vals
                              Nothing -> (expct `M.lookup` syndef) >>= snd

getAllArgs :: Options -> Expectation -> [String]
getAllArgs opts expct = let (syndef, pargs) = opts
                        in case expct `M.lookup` pargs of
                             Just vals -> vals
                             Nothing -> case (expct `M.lookup` syndef) >>= snd of
                               Just def -> [def]
                               Nothing -> []

getDefaultArg :: Options -> Expectation -> Maybe String
getDefaultArg opts expct = let (syndef, _) = opts
                           in (expct `M.lookup` syndef) >>= snd