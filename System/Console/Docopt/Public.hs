module System.Console.Docopt.Public 
  (
    -- everything locally declared
    module System.Console.Docopt.Public,

    -- public types
    Expectation(),
    Options(),
  )
  where

import System.Environment (getArgs)
import System.Exit

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
                                   Left err -> do putStrLn usageStr
                                                  exitFailure
                                   Right dop -> case getOptions dop rawargs of
                                       Left err         -> do putStrLn usageStr
                                                              exitFailure
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

isPresentM :: Monad m => Options -> Expectation -> m Bool
isPresentM o e = return $ isPresent o e

notPresent :: Options -> Expectation -> Bool
notPresent o e = not $ isPresent o e

notPresentM :: Monad m => Options -> Expectation -> m Bool
notPresentM o e = return $ not $ isPresent o e

getArg :: Monad m => Options -> Expectation -> m String
getArg opts expct = let (syndef, pargs) = opts
                    in case expct `M.lookup` pargs of
                          Just (val@(c:cs):vals) -> return val --ensure non-empty val
                          _ -> case expct `M.lookup` syndef of
                            Just (_, Just val) -> return val
                            _ -> fail $ "no argument given: " ++ show expct

getFirstArg :: Monad m => Options -> Expectation -> m String
getFirstArg opts expct = let (syndef, pargs) = opts
                             defaultVal = case expct `M.lookup` syndef of
                               Just (_, Just val) -> return val
                               _ -> fail $ "no argument given: " ++ show expct
                         in case expct `M.lookup` pargs of
                              Just vals@(_:_) -> case last vals of
                                "" -> defaultVal --if empty string, just use default
                                val -> return val
                              _ -> defaultVal


getAllArgs :: Options -> Expectation -> [String]
getAllArgs opts expct = let (syndef, pargs) = opts
                        in case expct `M.lookup` pargs of
                             Just vals -> vals
                             Nothing -> case (expct `M.lookup` syndef) >>= snd of
                               Just def -> [def]
                               Nothing -> []

getAllArgsM :: Monad m => Options -> Expectation -> m [String]
getAllArgsM o e = return $ getAllArgs o e

getDefaultArg :: Monad m => Options -> Expectation -> m String
getDefaultArg opts expct = let (syndef, _) = opts
                           in case expct `M.lookup` syndef of
                             Just (_, Just val) -> return val
                             _ -> fail ""


-- ** Public Expectation constructor functions

command :: String -> Expectation
command s = Command s

argument :: String -> Expectation
argument s = Argument s

shortOption :: Char -> Expectation
shortOption c = ShortOption c

longOption :: String -> Expectation
longOption s = LongOption s
