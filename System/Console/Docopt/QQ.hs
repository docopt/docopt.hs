{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}
module System.Console.Docopt.QQ
    (
    -- * QuasiQuoter usage parsers
      docopt
    , docoptFile
    ) where

import qualified Data.Map as M

import System.Console.Docopt.Types
import System.Console.Docopt.QQ.Instances ()
import System.Console.Docopt.ApplicativeParsec
import System.Console.Docopt.UsageParse

import Language.Haskell.TH
import Language.Haskell.TH.Quote

parseFmt :: FilePath -> String -> Either ParseError OptFormat
parseFmt = runParser pDocopt M.empty

docoptExp :: String -> Q Exp
docoptExp rawUsg = do
  let usg = trimEmptyLines rawUsg
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
--
-- For help with the docopt usage format, see
-- <https://github.com/docopt/docopt.hs/blob/master/README.md#help-text-format the readme on github>.
docopt :: QuasiQuoter
docopt = QuasiQuoter { quoteExp  = docoptExp
                     , quoteDec  = unsupported "Declaration"
                     , quotePat  = unsupported "Pattern"
                     , quoteType = unsupported "Type"
                     }
    where unsupported :: String -> String -> Q a
          unsupported qqType _ = do
            fail $ (qqType ++ " context unsupported")

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
-- string (relative to the directory from which ghc is invoked).
docoptFile :: QuasiQuoter
docoptFile = quoteFile docopt
