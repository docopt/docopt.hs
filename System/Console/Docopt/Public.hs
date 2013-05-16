module System.Console.Docopt.Public 
  (
    -- everything locally declared
    module System.Console.Docopt.Public,

    -- public types
    Expectation(),
    Options(),
    ParsingOptions(..),

    -- everything else
    defaultParsingOptions
  )
  where

import System.Environment (getArgs)
import System.Exit

import Data.Map as M

import Control.Applicative

import System.Console.Docopt.ParseUtils
import System.Console.Docopt.Types
import System.Console.Docopt.UsageParse (pDocopt)
import System.Console.Docopt.OptParse (getOptions)


-- * Public API

-- ** Main option parsing entry points

optionsWithUsage :: ParsingOptions -> String -> IO Options
optionsWithUsage options usage =
    do rawargs <- getArgs
       case runParser pDocopt M.empty "" usage of
           Left err  -> failure err
           Right dop -> case getOptions dop rawargs of
               Left err         -> failure err
               Right parsedOpts -> return parsedOpts

    where failure err = if showParseErrors options
                        then fail $ show err
                        else do putStrLn usage
                                exitFailure

optionsWithUsageFile :: ParsingOptions -> FilePath -> IO Options
optionsWithUsageFile options path = do usage <- readFile path
                                       optionsWithUsage options usage

-- ** Option lookup methods

isPresent :: Options -> Expectation -> Bool
isPresent opts expct = let (_, pargs) = opts
                       in case expct `M.lookup` pargs of
                              Just _ -> True
                              Nothing -> False

isPresentM :: Monad m => Options -> Expectation -> m Bool
isPresentM o e = return $ isPresent o e

notPresent :: Options -> Expectation -> Bool
notPresent o e = not $ isPresent o e

notPresentM :: Monad m => Options -> Expectation -> m Bool
notPresentM o e = return $ not $ isPresent o e

getArg :: Monad m => Options -> Expectation -> m String
getArg opts expct = let (syndef, pargs) = opts
                        failure = fail $ "no argument given: " ++ show expct
                    in case expct `M.lookup` pargs of
                          Just (val@(c:cs):vals) -> return val --ensure non-empty val
                          _ -> case expct `M.lookup` syndef of
                            Just syndef -> case defaultVal syndef of
                              Just def -> return def 
                              Nothing -> failure
                            _ -> failure

getFirstArg :: Monad m => Options -> Expectation -> m String
getFirstArg opts expct = let (syndef, pargs) = opts
                             failure = fail $ "no argument given: " ++ show expct
                             def = case expct `M.lookup` syndef of
                               Just syndef -> case defaultVal syndef of
                                 Just val -> return val
                                 Nothing -> failure
                               _ -> failure
                         in case expct `M.lookup` pargs of
                              Just vals@(_:_) -> case last vals of
                                "" -> def --if empty string, just use default
                                val -> return val
                              _ -> def

getArgWithDefault :: Options -> String -> Expectation -> String
getArgWithDefault opts def expct = case opts `getArg` expct of
                                      Just val -> val
                                      Nothing -> def

getAllArgs :: Options -> Expectation -> [String]
getAllArgs opts expct = let (syndef, pargs) = opts
                        in case expct `M.lookup` pargs of
                             Just vals -> vals
                             Nothing -> case (expct `M.lookup` syndef) >>= defaultVal of
                               Just def -> [def]
                               Nothing -> []

getAllArgsM :: Monad m => Options -> Expectation -> m [String]
getAllArgsM o e = return $ getAllArgs o e

-- is this lookup function even useful?
getDefaultArg :: Monad m => Options -> Expectation -> m String
getDefaultArg opts expct = let (syndefmap, _) = opts
                               failure = fail $ "no default argument given: " ++ show expct
                           in case expct `M.lookup` syndefmap of
                             Just syndef -> case defaultVal syndef of
                               Just val -> return val
                               Nothing -> failure
                             _ -> failure


-- ** Public Expectation constructor functions

command :: String -> Expectation
command s = Command s

argument :: String -> Expectation
argument s = Argument s

shortOption :: Char -> Expectation
shortOption c = ShortOption c

longOption :: String -> Expectation
longOption s = LongOption s
