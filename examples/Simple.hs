{-# LANGUAGE QuasiQuotes #-}
import System.Environment (getArgs)
import System.Console.Docopt

usageText :: Docopt
usageText = [docopt|
Usage:
  foo bar <baz>...
  foo --help

Options:
  -h, --help        Print this help message.
|]

main :: IO ()
main = do
  args <- parseArgsOrExit usageText =<< getArgs
  print args
  print (isPresent args (argument "baz")) 
