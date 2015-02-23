module System.Console.Docopt.Public 
  (
    -- everything locally declared
    module System.Console.Docopt.Public,

    -- public types
    Option(),
    Arguments(),
  )
  where

import System.Environment (getArgs)
import System.Exit

import Data.Map as M hiding (null)

import Control.Applicative

import System.Console.Docopt.ParseUtils
import System.Console.Docopt.Types
import System.Console.Docopt.UsageParse (pDocopt)
import System.Console.Docopt.OptParse (getArguments)


-- * Public API

-- ** Main option parsing entry points

optionsWithUsage :: String -> [String] -> IO Arguments
optionsWithUsage usage rawArgs = 
    case runParser pDocopt M.empty "Usage" usage of
        Left err -> do putStrLn usage
                       exitFailure
        Right fmt -> case getArguments fmt rawArgs of
            Left err         -> do putStrLn usage
                                   exitFailure
            Right parsedArgs -> return parsedArgs

optionsWithUsageDebug :: String -> [String] -> IO Arguments
optionsWithUsageDebug usage rawArgs =
    case runParser pDocopt M.empty "Usage" usage of
        Left err  -> fail $ show err
        Right fmt -> case getArguments fmt rawArgs of
            Left err         -> fail $ show err
            Right parsedArgs -> return parsedArgs


-- ** Option lookup functions

isPresent :: Arguments -> Option -> Bool
isPresent args opt = 
  case opt `M.lookup` args of
    Nothing  -> False
    Just val -> case val of
      NoValue    -> False
      NotPresent -> False
      _          -> True

{-# DEPRECATED isPresentM "Monadic query functions will soon be removed" #-}
isPresentM :: Monad m => Arguments -> Option -> m Bool
isPresentM args o = return $ isPresent args o

notPresent :: Arguments -> Option -> Bool
notPresent args o = not $ isPresent args o

{-# DEPRECATED notPresentM "Monadic query functions will soon be removed" #-}
notPresentM :: Monad m => Arguments -> Option -> m Bool
notPresentM args o = return $ not $ isPresent args o

getArg :: Monad m => Arguments -> Option -> m String
getArg args opt = 
  let failure = fail $ "no argument given: " ++ show opt
  in  case opt `M.lookup` args of
        Nothing  -> failure
        Just val -> case val of
          MultiValue (v:vs) -> return v
          Value v           -> return v
          _                 -> failure          

getFirstArg :: Monad m => Arguments -> Option -> m String
getFirstArg args opt = 
  let failure = fail $ "no argument given: " ++ show opt
  in  case opt `M.lookup` args of
        Nothing  -> failure
        Just val -> case val of
          MultiValue vs -> if null vs then failure else return $ last vs
          Value v       -> return v
          _             -> failure          


getArgWithDefault :: Arguments -> String -> Option -> String
getArgWithDefault args def opt = 
  case args `getArg` opt of
    Just val -> val
    Nothing -> def

getAllArgs :: Arguments -> Option -> [String]
getAllArgs args opt = 
  case opt `M.lookup` args of
     Nothing  -> []
     Just val -> case val of
       MultiValue vs -> reverse vs
       Value v       -> [v] 
       _             -> []

{-# DEPRECATED getAllArgsM "Monadic query functions will soon be removed" #-}
getAllArgsM :: Monad m => Arguments -> Option -> m [String]
getAllArgsM o e = return $ getAllArgs o e

getArgCount :: Arguments -> Option -> Int
getArgCount args opt = 
  case opt `M.lookup` args of
    Nothing -> 0
    Just val -> case val of
      Counted i     -> i
      MultiValue vs -> length vs
      Value _       -> 1
      Present       -> 1
      _             -> 0


-- ** Public Option constructor functions

command :: String -> Option
command s = Command s

argument :: String -> Option
argument s = Argument s

shortOption :: Char -> Option
shortOption c = ShortOption c

longOption :: String -> Option
longOption s = LongOption s
