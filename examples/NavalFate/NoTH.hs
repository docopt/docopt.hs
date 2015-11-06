import System.Environment (getArgs)
import System.Console.Docopt.NoTH
import NavalFate.Shared

main :: IO ()
main = do
  --args <- getArgs
  --print args

  -- ONLY for the sake of not repeating the naval fate usage
  usageStr <- readFile "NavalFate/USAGE.docopt"

  doc <- parseUsageOrDie usageStr
  args <- parseArgsOrDie doc =<< getArgs

  navalFateDispatchArgs doc args
