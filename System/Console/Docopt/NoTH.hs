module System.Console.Docopt.NoTH
  (
    -- * Usage parsers
      parseUsage
    , parseUsageOrDie

    , module System.Console.Docopt.Public
  )
  where

import Data.Map as M hiding (null)
import System.Exit

import System.Console.Docopt.Types
import System.Console.Docopt.Public
import System.Console.Docopt.ParseUtils
import System.Console.Docopt.UsageParse (pDocopt, trimEmptyLines)


-- | Parse docopt-formatted usage patterns.
--
--   For help with the docopt usage format, see
--   <https://github.com/docopt/docopt.hs/blob/master/README.md#help-text-format the readme on github>.
parseUsage :: String -> Either ParseError Docopt
parseUsage rawUsg =
  let usg = trimEmptyLines rawUsg
  in case runParser pDocopt M.empty "Usage" usg of
    Left e       -> Left e
    Right optfmt -> Right (Docopt optfmt usg)

-- | Same as 'parseUsage', but 'dieWithUsage' on parse failure. E.g.
--
-- > let usageStr = "Usage:\n  prog [--option]\n"
-- > patterns <- parseUsageOrDie usageStr
parseUsageOrDie :: String -> IO Docopt
parseUsageOrDie rawUsg = exitUnless $ parseUsage usg
  where
    usg = trimEmptyLines rawUsg
    die msg = putStr msg >> exitFailure
    exitUnless = either (const $ die usg) return
