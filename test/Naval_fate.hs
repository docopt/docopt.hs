import Control.Monad (when, unless)
import System.Environment (getArgs)
import System.Console.Docopt

main = do 
	--args <- getArgs
	--print args

	opts <- optionsWithUsageFile "naval_fate.USAGE.txt"
	--putStrLn "\nfinished parsing opts:\n"

	--print opts
	--putStrLn ""

	when (opts `isPresent` (command "ship")) $ do
		putStrLn "Command 'ship'"
		when (opts `isPresent` (command "new")) $ do
			putStrLn "  Command 'new'"
			name <- opts `getAllArgsM` (argument "name") 
			putStrLn $ "  <name> " ++ show name
		when (opts `isPresent` (command "shoot")) $ do
			putStrLn "  Command 'shoot'"
			x <- (opts `getArg` (argument "x"))
			putStrLn $ "  <x> " ++ show x
			y <- (opts `getArg` (argument "y"))
			putStrLn $ "  <y> " ++ show y
		when (opts `isPresent` (command "move")) $ do
			x <- opts `getArg` (argument "x")
			y <- opts `getArg` (argument "y")
			name <- opts `getArg` (argument "name")
			speed <- opts `getArg` (longOption "speed")
			putStrLn $ "<name> " ++ show name
			putStrLn "  Command 'move'"
			putStrLn $ "  <x> " ++ show x		
			putStrLn $ "  <y> " ++ show y
			putStrLn $ "  --speed=" ++ show speed
	when (opts `isPresent` (command "mine")) $ do
		putStrLn "Command 'mine'"
		x <- opts `getArg` (argument "x")
		y <- opts `getArg` (argument "y")
		isMoored <- opts `isPresentM` (longOption "moored")
		isDrifting <- opts `isPresentM` (longOption "drifting")
		when (opts `isPresent` (command "set")) $ do
			putStrLn "  Command 'set'"
		when (opts `isPresent` (command "remove")) $ do
			putStrLn "  Command 'remove'"
		putStrLn $ "  <x> " ++ show x		
		putStrLn $ "  <y> " ++ show y
		putStrLn $ if isMoored then "  --moored" else "  (not moored)"
		putStrLn $ if isDrifting then "  --drifting" else "  (not drifting)"
	when (opts `isPresent` (longOption "version")) $ do
		putStrLn "Naval Fate v0.0.0.0.0.1.0"
	when (opts `isPresent` (longOption "help")) $ do
		putStrLn =<< readFile "naval_fate.USAGE.txt"
