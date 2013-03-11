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

pShortOption :: CharParser u (Char, Bool)
pShortOption = try $ do char '-' 
                        ch <- letter 
                        expectsVal <- option False pOptionArgument
                        return (ch, expectsVal)

pLongOption :: CharParser u (Name, Bool)
pLongOption = try $ do string "--" 
                       name <- many1 $ oneOf alphanumerics
                       expectsVal <- option False pOptionArgument
                       return (name, expectsVal)

pAnyOption :: CharParser u String
pAnyOption = try (string "options")

pOptionArgument :: CharParser u Bool -- True if one is encountered, else False
pOptionArgument = try $ do char '=' <|> inlineSpace
                           pArgument <|> many1 (oneOf uppers)
                           return True
                  <|> return False

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
               <|> ShortOption . fst <$> pShortOption
               <|> LongOption . fst <$> pLongOption
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

pOptSynonyms :: CharParser u ([Expectation], Bool)
pOptSynonyms = do inlineSpaces 
                  pairs <- p `sepEndBy1` (optional (char ',') >> inlineSpace)
                  let expectations = map fst pairs
                      expectsVal = or $ map snd pairs
                  return (expectations, expectsVal)
             where p =   (\(c, ev) -> (ShortOption c, ev)) <$> pShortOption
                     <|> (\(s, ev) -> (LongOption s, ev)) <$> pLongOption

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

pOptDescription :: CharParser SynDefMap ()
pOptDescription = try $ do
    (syns, expectsVal) <- pOptSynonyms
    def <- pOptDefault
    skipUntil (newline >> begOptionLine)
    updateState $ \synmap -> 
      let syndef = SynonymDefault {synonyms = syns, defaultVal = def, expectsVal = expectsVal}
          saveSynDef mp expct = M.insert expct syndef mp
      in  foldl saveSynDef synmap syns 
    return ()

pOptDescriptions :: CharParser SynDefMap SynDefMap
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
pDocopt :: CharParser SynDefMap Docopt
pDocopt = do
    expct <- pUsagePatterns
    optSynsDefs <- pOptDescriptions
    return (expct, optSynsDefs)


