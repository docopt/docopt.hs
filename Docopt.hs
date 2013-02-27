module Docopt where

import ApplicativeParsec
import Data.Char (isSpace)

import           Data.Map (Map)
import qualified Data.Map as M

import Data.List (intercalate)
import System.Environment (getArgs)


-- * Usage expression Types

type Name = String

data Expectation = Sequence [Expectation]   -- firstarg secondarg thirdarg
                 | OneOf [Expectation]      -- --flag | --other
                 | Optional Expectation     -- [--flag]
                 | Repeated Expectation     -- args...
                 | ShortOption Char         -- -f
                 | LongOption Name          -- --flag
                 | AnyOption                -- [options]
                 | Argument Name            -- <name>
                 | Command Name             -- commandname
                 deriving (Show, Eq, Ord)

-- * Main Functions

main :: IO ()
main = do
    contents <- getContents
    putStr $ unlines $ map (show . parseDocopt) $ lines contents

testDocopt :: IO ()
testDocopt = do
    contents <- readFile "test_usage.txt"
    print $ runParser pDocopt M.empty "" contents


-- * Constants

letters = ['a'..'z']++['A'..'Z']
alphanumerics = letters++['0'..'9']++['-','_']
programNameChars = alphanumerics++"./~"


-- * Helpers

-- |Flattens the top level of an `Expectation`, as long as that 
-- /does not/ alter the matching semantics of the `Expectation`
flatten :: Expectation -> Expectation
flatten (Sequence (e:[])) = e
flatten (OneOf (e:[]))    = e
flatten e                 = e

flatSequence = flatten . Sequence
flatOneOf = flatten . OneOf


-- * Basic Parsers

isInlineSpace :: Char -> Bool
isInlineSpace c = not (c `elem` "\n\r") 
               && (isSpace c)

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


-- * Expectation Parsers 

pLine :: CharParser u Expectation
pLine = flatten . OneOf <$> pExpSeq `sepBy1` (inlineSpaces >> pipe)

pExpSeq :: CharParser u Expectation
pExpSeq = flatten . Sequence <$> (pExp `sepEndBy1` inlineSpaces1)

pOptGroup :: CharParser u [Expectation]
pOptGroup = pGroup '[' pExpSeq ']'

pReqGroup :: CharParser u [Expectation]
pReqGroup = pGroup '(' pExpSeq ')'

pShortOption :: CharParser u Char
pShortOption = try (char '-' >> letter) <* optional pOptionArgument

pLongOption :: CharParser u Name
pLongOption = try (string "--" >> (many1 $ oneOf alphanumerics)) <* optional pOptionArgument

pAnyOption :: CharParser u String
pAnyOption = try (string "options")

pOptionArgument :: CharParser u ()
pOptionArgument = try $ do
    char '=' <|> inlineSpace
    pArgument
    return ()

pArgument :: CharParser u String
pArgument = between (char '<') (char '>') pCommand

pCommand :: CharParser u String
pCommand = many1 (oneOf alphanumerics)

-- '<arg>...' make an Expectation Repeated if followed by ellipsis
repeatable :: CharParser u Expectation -> CharParser u Expectation
repeatable p = do 
    expct <- p
    tryRepeat <- ((try ellipsis) >> (return Repeated)) <|> (return id)
    return (tryRepeat expct)

pExp :: CharParser u Expectation
pExp = inlineSpaces >> repeatable value
     where value = Optional . flatOneOf <$> pOptGroup
               <|> flatOneOf <$> pReqGroup
               <|> ShortOption <$> pShortOption
               <|> LongOption <$> pLongOption
               <|> return (Repeated AnyOption) <* pAnyOption
               <|> Argument <$> pArgument
               <|> Command <$> pCommand

parseDocopt :: String -> Either ParseError Expectation
parseDocopt = parse pLine ""


-- * Usage Pattern Parsers

pUsageHeader :: CharParser u String
pUsageHeader = try $ do 
    u <- (char 'U' <|> char 'u')
    sage <- string "sage:"
    return (u:sage)

-- | Ignores leading spaces and first word, then parses
--   the rest of the usage line
pUsageLine :: CharParser u Expectation
pUsageLine = try $ do
                inlineSpaces >> many1 (satisfy (not . isSpace))
                pLine

pUsagePatterns :: CharParser u Expectation
pUsagePatterns = do
        (skipUntil pUsageHeader) >> pUsageHeader
        optionalEndline
        flatten . OneOf <$> (pUsageLine `sepEndBy1` endline)


-- * Option Aliases & Defaults Parsers

-- | A Map from key expectation (long or short option) to value
--   of a tuple whose first element is a list of synonymous expectations
--   (e.g. -h, --help) and whose second element is Maybe a default value
--   for those synonymous expectations
type OptSynonymsDefaults = Map Expectation ([Expectation], (Maybe String))

-- | Succeeds only on the first line of an option explanation
--   (one whose first non-space character is '-')
begOptionLine :: CharParser u String
begOptionLine = inlineSpaces >> lookAhead (char '-') >> return "-"

pOptSynonyms :: CharParser u [Expectation]
pOptSynonyms = inlineSpaces >> p `sepEndBy1` (optional (char ',') >> inlineSpace)
             where p = ShortOption <$> pShortOption
                     <|> LongOption <$> pLongOption

pDefaultTag :: CharParser u String
pDefaultTag = do
    string "[default:"
    inlineSpaces
    def <- many (noneOf "]")
    char ']'
    return def

pOptDefault :: CharParser u (Maybe String)
pOptDefault = do
    skipUntil (pDefaultTag <|> (newline >> begOptionLine))
    maybeDefault <- optionMaybe pDefaultTag
    return maybeDefault

pOptDescription :: CharParser OptSynonymsDefaults ()
pOptDescription = try $ do
    syns <- pOptSynonyms
    def <- pOptDefault
    skipUntil (newline >> begOptionLine)
    updateState $ \synmap -> 
      let f mp expct = M.insert expct (syns, def) mp
      in  foldl f synmap syns 
    return ()

pOptDescriptions :: CharParser OptSynonymsDefaults OptSynonymsDefaults
pOptDescriptions = do
    skipUntil (newline >> begOptionLine)
    newline
    setState M.empty
    pOptDescription `sepEndBy` endline
    getState


-- * Docopt Types, Parser

type DocoptParser = (Expectation, OptSynonymsDefaults)

-- | Main usage parser: parses all of the usage lines into an Exception,
--   and all of the option descriptions along with any accompanying 
--   defaults, and returns both in a tuple
pDocopt :: CharParser OptSynonymsDefaults DocoptParser
pDocopt = do
    expct <- pUsagePatterns
    optSynsDefs <- pOptDescriptions
    return (expct, optSynsDefs)


type ParsedArguments = Map Expectation [String]
type Options = (OptSynonymsDefaults, ParsedArguments)

getOptions :: DocoptParser -> [String] -> Either ParseError Options
getOptions dop rawargs = let (expct, syndef) = dop
                             delim = "«»"
                             p = buildOptParser delim expct
                         in runParser p (syndef, M.empty) "" (delim `intercalate` rawargs)

-- | The meat and potatoes.
buildOptParser :: String ->
                  -- ^ an obscure delimiter with which to intercalate the args list
                  Expectation -> 
                  -- ^ the expected form of the options 
                  CharParser Options Options
                  -- ^ a CharParser with which a ParsedArguments (k,v) list can be built
buildOptParser delim (Sequence exs)      = foldl1 f ps 
                                           where ps = (buildOptParser delim) `map` exs
                                                 f = \p1 p2 -> p1 >> string delim >> p2
buildOptParser delim (OneOf exs)         = choice $ (buildOptParser delim) `map` exs
buildOptParser delim (Optional ex)       = do optional $ try (buildOptParser delim ex) 
                                              getState
buildOptParser delim (Repeated ex)       = do many1 $ buildOptParser delim ex
                                              getState
buildOptParser delim e@(ShortOption c)   = do (char '-' >> char c)
                                              optional (char '=') <|> optional (string delim)
                                              val <- many (notFollowedBy (string delim) >> anyChar)
                                              updateState $ withEachSynonym e $
                                                            \pa syn -> saveOccurrence syn val pa
                                              getState
buildOptParser delim e@(LongOption name) = do (string "--" >> string name)
                                              optional (char '=') <|> optional (string delim)
                                              val <- many (notFollowedBy (string delim) >> anyChar)
                                              updateState $ withEachSynonym e $
                                                            \pa syn -> saveOccurrence syn val pa
                                              getState
buildOptParser delim (AnyOption)         = undefined
buildOptParser delim e@(Argument name)   = do val <- many1 (notFollowedBy (string delim) >> anyChar)
                                              updateState $ updateParsedArgs $ saveOccurrence e val
                                              getState
buildOptParser delim e@(Command name)    = do string name
                                              updateState $ updateParsedArgs $ assertPresent e
                                              getState

updateParsedArgs :: (ParsedArguments -> ParsedArguments) -> Options -> Options
updateParsedArgs f (syndef, pa) = (syndef, f pa)

saveOccurrence :: Expectation -> String -> ParsedArguments -> ParsedArguments
saveOccurrence e val pa = M.insertWith f e [val] pa
    where f newval oldval = newval ++ oldval

assertPresent :: Expectation -> ParsedArguments -> ParsedArguments
assertPresent e pa = M.insertWith f e [""] pa
    where f newval oldval = oldval 
    -- TODO make this more robust, don't insert multiple empty strings

withEachSynonym :: Expectation -> 
                   (ParsedArguments -> Expectation -> ParsedArguments) -> 
                   Options -> 
                   Options
withEachSynonym ex savef o = let (syndef, pa) = o
                                 syns = fst $ M.findWithDefault ([], Nothing) ex syndef
                             in (syndef, foldl savef pa syns)