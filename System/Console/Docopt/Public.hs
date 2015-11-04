module System.Console.Docopt.Public
  (
    -- * Command line arguments parsers
      parseArgs
    , parseArgsOrDie

    -- *** Re-exported from Parsec
    , ParseError

    -- * Parsed usage string
    , Docopt ()
    , usage
    , exitWithUsage
    , dieWithUsage
    , exitWithUsageMessage
    , dieWithUsageMessage

    -- * Argument lookup
    , Option()
    , Arguments()

    -- ** Query functions
    , isPresent
    , notPresent
    , getArg
    , getArgOrDieWith
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
  )
  where

import System.Exit

import Data.Map as M hiding (null)
import Data.Maybe (fromMaybe)
import System.Console.Docopt.Types
import System.Console.Docopt.ApplicativeParsec (ParseError)
import System.Console.Docopt.OptParse
import System.IO

-- | Parse command line arguments.
parseArgs :: Docopt -> [String] -> Either ParseError Arguments
parseArgs parser = getArguments (optFormat parser)

-- | Same as 'parseArgs', but 'dieWithUsage' on parse failure. E.g.
--
-- > args <- parseArgsOrDie patterns =<< getArgs
parseArgsOrDie :: Docopt -> [String] -> IO Arguments
parseArgsOrDie parser argv = either (const $ dieWithUsage parser) return $ parseArgs parser argv

-- | Exit after printing usage text to stdout.
exitWithUsage :: Docopt -> IO a
exitWithUsage doc = do
  putStr $ usage doc
  exitSuccess

-- | Exit after printing usage text to stderr.
dieWithUsage :: Docopt -> IO a
dieWithUsage = die . usage

-- | Exit after printing a custom message followed by usage text to stdout.
--   Intended for providing documentation to a user.
exitWithUsageMessage :: Docopt -> String -> IO a
exitWithUsageMessage doc msg = do
  putStrLn msg
  putStrLn ""
  exitWithUsage doc

-- | Exit after printing a custom message followed by usage text to stderr.
--   Intended for when something went wrong and context can be given about
--   what went wrong.
dieWithUsageMessage :: Docopt -> String -> IO a
dieWithUsageMessage doc msg = do
  hPutStrLn stderr msg
  hPutStrLn stderr ""
  dieWithUsage doc


-- Query functions
------------------

-- | 'True' if an option was present at all in an invocation.
--
--   Useful with 'longOption's and 'shortOption's, and in conjunction with 'Control.Monad.when'.
isPresent :: Arguments -> Option -> Bool
isPresent args opt =
  case opt `M.lookup` args of
    Nothing  -> False
    Just val -> case val of
      NoValue       -> False
      NotPresent    -> False
      Counted 0     -> False
      MultiValue [] -> False
      _             -> True

notPresent :: Arguments -> Option -> Bool
notPresent = (not .) . isPresent

-- | 'Just' the value of the argument supplied, or 'Nothing' if one was not given.
--
--   If the option's presence is required by your 'Docopt' usage text
--   (e.g. a positional argument), as in
--
-- > Usage:
-- >   prog <required>
--
--   then @getArg args (argument \'required\')@ is guaranteed to be a 'Just'.
getArg :: Arguments -> Option -> Maybe String
getArg args opt =
  case opt `M.lookup` args of
    Nothing  -> Nothing
    Just val -> case val of
      MultiValue (v:_) -> Just v
      Value v          -> Just v
      _                -> Nothing

-- | Same as 'getArg', but 'dieWithUsage' if 'Nothing'.
--
--   As in 'getArg', if your usage pattern required the option, 'getArgOrExitWith' will not exit.
getArgOrDieWith :: Docopt -> Arguments -> Option -> IO String
getArgOrDieWith doc args opt = dieUnless $ getArg args opt
  where dieUnless = maybe (dieWithUsageMessage doc $ "argument expected for: " ++ show opt) return

-- | Same as 'getArg', but eliminate 'Nothing' with a default argument.
getArgWithDefault :: Arguments -> String -> Option -> String
getArgWithDefault args def opt = fromMaybe def (args `getArg` opt)

-- | Returns all occurrences of a repeatable option, e.g. @\<file\>...@.
getAllArgs :: Arguments -> Option -> [String]
getAllArgs args opt =
  case opt `M.lookup` args of
    Nothing  -> []
    Just val -> case val of
      MultiValue vs -> reverse vs
      Value v       -> [v]
      _             -> []

-- | Return the number of occurrences of an option in an invocation.
--
--   Useful with repeatable flags, e.g. @[ -v | -vv | -vvv]@.
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

-- | For @Usage: prog cmd@, ask for @command \"cmd\"@.
command :: String -> Option
command = Command

-- | For @Usage: prog \<file\>@, ask for @argument \"file\"@.
--
--   __Note:__ A @Usage: prog --output=\<file\>@ is /not/ matched by @argument \"file\"@. See 'longOption'.
argument :: String -> Option
argument = Argument

-- | For @Usage: prog -h@, ask for @shortOption \'h\'@.
--
--   For @Usage: prog -o=\<file\>@, ask for @shortOption \'o\'@.
shortOption :: Char -> Option
shortOption = ShortOption

-- | For @Usage: prog --version@, ask for @longOption \"version\"@.
--
--   For @Usage: prog --output=\<file\>@, ask for @longOption \"output\"@.
longOption :: String -> Option
longOption = LongOption

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
