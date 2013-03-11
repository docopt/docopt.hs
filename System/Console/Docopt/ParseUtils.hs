module System.Console.Docopt.ParseUtils
	(
		module System.Console.Docopt.ParseUtils,
		module System.Console.Docopt.ApplicativeParsec,
		isSpace,
	)
	where

import System.Console.Docopt.ApplicativeParsec
import Data.Char (isSpace)

-- * Constants

lowers = ['a'..'z']
uppers = ['A'..'Z']
letters = lowers++uppers
alphanumerics = letters++['0'..'9']++['-','_']


-- * Basic Parsers

isInlineSpace :: Char -> Bool
isInlineSpace c = not (c `elem` "\n\r") 
               	   && (isSpace c)

inlineSpace :: CharParser u Char
inlineSpace = satisfy isInlineSpace 

-- | like `spaces`, except does not match newlines
inlineSpaces :: CharParser u ()
inlineSpaces = skipMany (satisfy isInlineSpace)

inlineSpaces1 :: CharParser u ()
inlineSpaces1 = skipMany1 (satisfy isInlineSpace)

spaces1 :: CharParser u ()
spaces1 = skipMany1 (satisfy isSpace)

endline = inlineSpaces >> newline
optionalEndline = inlineSpaces >> (optional newline)

pipe = char '|'

ellipsis :: CharParser u String
ellipsis = string "..."

-- |@skipUntil p@ ignores everything that comes before `p`. 
-- Returns what `p` returns.
skipUntil :: Show a => CharParser u a -> CharParser u ()
skipUntil p = skipMany (notFollowedBy p >> anyChar)

pGroup :: Char -> CharParser u a -> Char -> CharParser u [a]
pGroup beg elemParser end = between (char beg) (inlineSpaces >> char end) 
                            $ (inlineSpaces >> notFollowedBy pipe >> elemParser) 
                              `sepBy`
                              (inlineSpaces >> pipe)


