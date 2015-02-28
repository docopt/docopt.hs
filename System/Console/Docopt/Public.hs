module System.Console.Docopt.Public
  (
    -- * Parsed usage string
      Docopt ()
    , usage
    , exitWithUsage
    , exitWithUsageMessage

    -- * Argument lookup
    , Option()
    , Arguments()

    -- ** Query functions
    , isPresent
    , notPresent
    , getArg
    , getArgWithDefault
    , getAllArgs
    , getArgCount

    -- ** 'Option' constructors
    , command
    , argument
    , shortOption
    , longOption

    -- ** Deprecated
    , getAllArgsM
    , notPresentM
    , isPresentM
    , getFirstArg

    -- ** Re-exported from Parsec
    , ParseError
  )
  where

import System.Exit

import Data.Map as M hiding (null)
import Data.Maybe (fromMaybe)
import System.Console.Docopt.Types
import System.Console.Docopt.ApplicativeParsec (ParseError)


-- | Exit after printing usage text.
exitWithUsage :: Docopt -> IO a
exitWithUsage doc = do
  putStr $ usage doc
  exitFailure

-- | Exit after printing a custom message followed by usage text.
--   Intended for convenience when more context can be given about what went wrong.
exitWithUsageMessage :: Docopt -> String -> IO a
exitWithUsageMessage doc msg = do
  putStrLn msg
  putStrLn ""
  exitWithUsage doc

-- Query functions
------------------

isPresent :: Arguments -> Option -> Bool
isPresent args opt =
  case opt `M.lookup` args of
    Nothing  -> False
    Just val -> case val of
      NoValue    -> False
      NotPresent -> False
      _          -> True

notPresent :: Arguments -> Option -> Bool
notPresent args o = not $ isPresent args o

getArg :: Arguments -> Option -> Maybe String
getArg args opt =
  case opt `M.lookup` args of
    Nothing  -> Nothing
    Just val -> case val of
      MultiValue (v:_) -> Just v
      Value v          -> Just v
      _                -> Nothing

getArgWithDefault :: Arguments -> String -> Option -> String
getArgWithDefault args def opt = fromMaybe def (args `getArg` opt)

getAllArgs :: Arguments -> Option -> [String]
getAllArgs args opt =
  case opt `M.lookup` args of
    Nothing  -> []
    Just val -> case val of
      MultiValue vs -> reverse vs
      Value v       -> [v]
      _             -> []

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


-- Option constructors
----------------------

command :: String -> Option
command s = Command s

argument :: String -> Option
argument s = Argument s

shortOption :: Char -> Option
shortOption c = ShortOption c

longOption :: String -> Option
longOption s = LongOption s

-- Deprecated
-------------

{-# DEPRECATED getAllArgsM "Monadic query functions will soon be removed" #-}
getAllArgsM :: Monad m => Arguments -> Option -> m [String]
getAllArgsM o e = return $ getAllArgs o e

{-# DEPRECATED notPresentM "Monadic query functions will soon be removed" #-}
notPresentM :: Monad m => Arguments -> Option -> m Bool
notPresentM args o = return $ not $ isPresent args o

{-# DEPRECATED isPresentM "Monadic query functions will soon be removed" #-}
isPresentM :: Monad m => Arguments -> Option -> m Bool
isPresentM args o = return $ isPresent args o

{-# DEPRECATED getFirstArg "Use 'getAllArgs' instead" #-}
getFirstArg :: Monad m => Arguments -> Option -> m String
getFirstArg args opt =
  let failure = fail $ "no argument given: " ++ show opt
  in  case opt `M.lookup` args of
        Nothing  -> failure
        Just val -> case val of
          MultiValue vs -> if null vs then failure else return $ last vs
          Value v       -> return v
          _             -> failure
