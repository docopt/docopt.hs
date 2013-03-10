module System.Console.Docopt.UsageParse 
  where

import qualified Data.Map as M

import System.Console.Docopt.ParseUtils
import System.Console.Docopt.Types

-- * Helpers

-- |Flattens the top level of an `Expectation`, as long as that 
-- /does not/ alter the matching semantics of the `Expectation`
flatten :: Expectation -> Expectation
flatten (Sequence (e:[])) = e
flatten (OneOf (e:[]))    = e
flatten e                 = e

flatSequence = flatten . Sequence
flatOneOf = flatten . OneOf


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

-- * Option Synonyms & Defaults Parsers

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

-- | Main usage parser: parses all of the usage lines into an Exception,
--   and all of the option descriptions along with any accompanying 
--   defaults, and returns both in a tuple
pDocopt :: CharParser OptSynonymsDefaults Docopt
pDocopt = do
    expct <- pUsagePatterns
    optSynsDefs <- pOptDescriptions
    return (expct, optSynsDefs)


