import Control.Monad (when, unless)
import System.Environment (getArgs)
import System.Exit 
import System.Console.Docopt

usage = "Naval Fate.\n\n\
\Usage:\n\
\  naval_fate ship new <name>...\n\
\  naval_fate ship shoot <x> <y>\n\
\  naval_fate mine (set|remove) <x> <y> [--moored | --drifting]\n\
\  naval_fate mine list [options]\n\
\  naval_fate mine show [-md]\n\
\  naval_fate ship <name> move <x> <y> [--speed=<kn>]\n\
\  naval_fate --help | -h\n\
\  naval_fate --version\n\n\
\Options:\n\
\  -h, --help      Show this screen.\n\
\  --version       Show version.\n\
\  --speed=<kn>    Speed in knots [default: 10].\n\
\  -m, --moored    Moored (anchored) mine.\n\
\  -d, --drifting  Drifting mine."

testOptions = defaultParsingOptions { showParseErrors = True }

main = do 
	--args <- getArgs
	--print args

	opts <- optionsWithUsage testOptions usage

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
		when (or $ map (opts `isPresent`) [command "set", command "remove"]) $ do
			when (opts `isPresent` (command "set")) $ do
				putStrLn "  Command 'set'"
			when (opts `isPresent` (command "remove")) $ do
				putStrLn "  Command 'remove'"
			x <- opts `getArg` (argument "x")
			y <- opts `getArg` (argument "y")
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
		putStrLn usage
