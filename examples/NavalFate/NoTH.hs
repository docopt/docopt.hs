import System.Environment (getArgs)
import System.Console.Docopt
import NavalFate.Shared

main :: IO ()
main = do
  --args <- getArgs
  --print args

  opts <- optionsWithUsageFileDebug "NavalFate/USAGE.docopt"

  navalFateDispatchArgs opts
