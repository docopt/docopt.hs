module System.Console.Docopt.ParseUtils
    (
        module System.Console.Docopt.ParseUtils,
        module System.Console.Docopt.ApplicativeParsec,
        module Data.Char,
    )
    where

import System.Console.Docopt.ApplicativeParsec

import           Data.Map (Map)
import qualified Data.Map as M

import Data.Char (isSpace, toUpper, toLower)

-- * Constants

lowers = ['a'..'z']
uppers = ['A'..'Z']
letters = lowers++uppers
numerics = ['0'..'9']++"-_"
specialChars = " :/"
alphanumerics = letters++numerics
alphanumSpecial = alphanumerics ++ specialChars


-- * Basic Parsers

caseInsensitive :: String -> CharParser u String
caseInsensitive = sequence . (map (\c -> (char $ toLower c) <|> (char $ toUpper c)))

lookAhead_ :: CharParser u a -> CharParser u ()
lookAhead_ p = do lookAhead p
                  return ()

isNotFollowedBy :: Show a => CharParser u a -> CharParser u Bool
isNotFollowedBy p = option False (notFollowedBy p >> return True)

isInlineSpace :: Char -> Bool
isInlineSpace c = not (c `elem` "\n\r") 
                   && (isSpace c)

inlineSpace :: CharParser u Char
inlineSpace = satisfy isInlineSpace
            <?> "inline-space"

-- | like `spaces`, except does not match newlines
inlineSpaces :: CharParser u ()
inlineSpaces = skipMany (satisfy isInlineSpace)
             <?> "inline-spaces"

inlineSpaces1 :: CharParser u ()
inlineSpaces1 = skipMany1 (satisfy isInlineSpace)
              <?> "1+ inline-spaces"

spaces1 :: CharParser u ()
spaces1 = skipMany1 (satisfy isSpace)
        <?> ">=1 spaces"

endline = inlineSpaces >> newline
optionalEndline = inlineSpaces >> (optional newline)

pipe = char '|' <?> "'|'"

ellipsis :: CharParser u String
ellipsis = inlineSpaces >> string "..."
         <?> "'...'"


manyTill1 p end = do
  first <- p 
  rest <- manyTill p end
  return $ first : rest

-- |@skipUntil p@ ignores everything that comes before `p`. 
-- Returns what `p` returns.
skipUntil :: Show a => CharParser u a -> CharParser u ()
skipUntil p = skipMany (notFollowedBy p >> anyChar)

pGroup :: Char -> CharParser u a -> Char -> CharParser u [a]
pGroup beg elemParser end = between (char beg) (inlineSpaces >> char end) 
                            $ (inlineSpaces >> notFollowedBy pipe >> elemParser) 
                              `sepBy`
                              (inlineSpaces >> pipe)

betweenS :: String -> String -> CharParser u a -> CharParser u [a]
betweenS b e p = between begin end manyP
                 where begin = try $ string b
                       end = try $ inlineSpaces >> (string e)
                       manyP = p `sepBy` inlineSpaces1


-- | Data.Map utils
alterAllWithKey :: Ord k => (k -> Maybe a -> Maybe a) -> [k] -> Map k a -> Map k a
alterAllWithKey f ks m = foldl (\m' k -> M.alter (f k) k m') m ks
