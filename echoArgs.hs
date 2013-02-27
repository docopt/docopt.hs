import Data.List (intercalate)
import System.Environment (getArgs)

join = intercalate

main = do
	args <- getArgs
	print args
	putStrLn $ join "«»" args