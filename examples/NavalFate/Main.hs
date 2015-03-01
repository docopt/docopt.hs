{-# LANGUAGE QuasiQuotes #-}
import System.Environment (getArgs)
import System.Console.Docopt
import NavalFate.Shared

navalFateUsage :: Docopt
navalFateUsage = [docoptFile|NavalFate/USAGE.docopt|]

main :: IO ()
main = do
  --args <- getArgs
  --print args

  opts <- parseArgsOrExit navalFateUsage =<< getArgs

  navalFateDispatchArgs navalFateUsage opts
