{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}
-- | Template Haskell 'QuasiQuoter's which enable compile time parsing
-- of usage strings.
--
-- Example:
--
-- @
-- {-\# LANGUAGE QuasiQuotes \#-}
-- module Main where
--
-- import Control.Monad (when)
-- import Data.Char (toUpper)
-- import System.Console.Docopt.QQ
--
-- patterns :: Docopt
-- patterns = [docopt|
-- docopt-sample version 0.1.0
--
-- Usage:
--   docopt-sample cat \<file\>
--   docopt-sample echo [--caps] \<string\>
--
-- Options:
--   -c, --caps    Caps-lock the echoed argument
-- |]
--
-- main :: IO ()
-- main = do
--   args <- parseArgs' patterns
--
--   when (args \`isPresent\` (command \"cat\")) $ do
--     file <- args \`getArg\` (argument \"file\")
--     putStr =<< readFile file
--
--   when (args \`isPresent\` (command \"echo\")) $ do
--     let charTransform = if args \`isPresent\` (longOption \"caps\")
--                         then toUpper
--                         else id
--     string <- args \`getArg\` (argument \"string\")
--     putStrLn $ map charTransform string
-- @
module System.Console.Docopt.QQ
    (
    -- * QuasiQuoters
      docopt
    , docoptFile

    -- * Command line arguments parsers
    , parseArgs
    , parseArgsOrExit
    ) where

import Control.Applicative ((<$>))

import System.Console.Docopt.Types
import System.Console.Docopt.QQ.Util
import System.Console.Docopt.QQ.Instances ()

import Language.Haskell.TH
import Language.Haskell.TH.Quote

docoptExp :: String -> Q Exp
docoptExp usg = do
  let mkDocopt fmt = Docopt { usage = usg, optFormat = fmt }
  loc <- loc_filename <$> location
  case mkDocopt <$> parseFmt loc usg of
    Left err     -> fail $ show err
    Right parser -> [| parser |]

-- | A 'QuasiQuoter' which parses a usage string and returns a
-- 'Docopt'.
--
-- Example usage:
--
-- @
-- patterns :: Docopt
-- patterns = [docopt|
-- docopt-sample version 0.1.0
--
-- Usage:
--   docopt-sample cat \<file\>
--   docopt-sample echo [--caps] \<string\>
--
-- Options:
--   -c, --caps    Caps-lock the echoed argument
-- |]
-- @
docopt :: QuasiQuoter
docopt = QuasiQuoter { quoteExp  = docoptExp
                     , quoteDec  = unsupported "Declaration"
                     , quotePat  = unsupported "Pattern"
                     , quoteType = unsupported "Type"
                     }
    where unsupported = fail . (++ " context unsupported")

-- | Same as 'docopt', but parses the given file instead of a literal
-- string.
--
-- Example:
--
-- @
-- patterns :: Docopt
-- patterns = [docoptFile|USAGE|]
-- @
--
-- where @USAGE@ is the name of a file which contains the usage
-- string.
docoptFile :: QuasiQuoter
docoptFile = quoteFile docopt
