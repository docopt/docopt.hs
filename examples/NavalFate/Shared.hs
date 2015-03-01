module NavalFate.Shared where

import System.Console.Docopt
import System.Exit
import Control.Monad (when)

navalFateDispatchArgs :: Docopt -> Arguments -> IO ()
navalFateDispatchArgs doc opts = do
  -- print opts
  -- putStrLn ""

  let getArgOrExit = getArgOrExitWith doc

  when (opts `isPresent` (command "ship")) $ do
    putStrLn "Command 'ship'"
    when (opts `isPresent` (command "new")) $ do
      putStrLn "  Command 'new'"
      let name = opts `getAllArgs` (argument "name")
      putStrLn $ "  <name> " ++ show name
    when (opts `isPresent` (command "shoot")) $ do
      putStrLn "  Command 'shoot'"
      x <- (opts `getArgOrExit` (argument "x"))
      putStrLn $ "  <x> " ++ show x
      y <- (opts `getArgOrExit` (argument "y"))
      putStrLn $ "  <y> " ++ show y
    when (opts `isPresent` (command "move")) $ do
      x <- opts `getArgOrExit` (argument "x")
      y <- opts `getArgOrExit` (argument "y")
      name <- opts `getArgOrExit` (argument "name")
      speed <- opts `getArgOrExit` (longOption "speed")
      putStrLn $ "<name> " ++ show name
      putStrLn "  Command 'move'"
      putStrLn $ "  <x> " ++ show x
      putStrLn $ "  <y> " ++ show y
      putStrLn $ "  --speed=" ++ show speed
  when (opts `isPresent` (command "mine")) $ do
    putStrLn "Command 'mine'"
    when (or $ map (opts `isPresent`) [command "set", command "remove"]) $ do
      when (opts `isPresent` (command "set")) $ do
        putStrLn "  Command 'set'"
      when (opts `isPresent` (command "remove")) $ do
        putStrLn "  Command 'remove'"
      x <- opts `getArgOrExit` (argument "x")
      y <- opts `getArgOrExit` (argument "y")
      putStrLn $ "  <x> " ++ show x
      putStrLn $ "  <y> " ++ show y
    when (opts `isPresent` (command "list")) $ do
      putStrLn "  Command 'list'"
    when (opts `isPresent` (command "show")) $ do
      putStrLn "  Command 'show'"
    when (opts `isPresent` (longOption "moored")) $ do
      putStrLn "  --moored"
    when (opts `isPresent` (longOption "drifting")) $ do
      putStrLn "  --drifting"
    exitSuccess
  when (opts `isPresent` (longOption "version")) $ do
    putStrLn "Naval Fate v0.0.0.0.0.1.0"
  when (opts `isPresent` (longOption "help")) $ do
    exitWithUsage doc
