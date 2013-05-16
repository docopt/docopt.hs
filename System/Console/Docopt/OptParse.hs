module System.Console.Docopt.OptParse 
  where

import Control.Monad (unless)

import qualified Data.Map as M
import           Data.List (intercalate, nub, (\\))

import System.Console.Docopt.ParseUtils
import System.Console.Docopt.Types


-- | The meat and potatoes.
buildOptParser :: String ->
                  -- ^ an obscure delimiter with which to intercalate the args list
                  OptFormat -> 
                  -- ^ the expected form of the options 
                  CharParser OptParserState ()
                  -- ^ a CharParser with which a ParsedArguments (k,v) list can be built
buildOptParser delim fmt@(pattern, infomap) = 
  
  let -- Helpers
      argDelim = (try $ string delim) <?> "space between arguments"
      
      makeParser p = buildOptParser delim (p, infomap)
      
      argDelimIfNotInShortOptStack = do 
        st <- getState 
        if not $ inShortOptStack st
          then optional argDelim
          else return ()

      updateOptWith :: (Option -> OptionInfo -> String -> Arguments -> Arguments) ->
                       Option ->
                       String ->
                       CharParser OptParserState ()
      updateOptWith updateFn opt val = do
        st <- getState
        let optInfo = (optInfoMap st) M.! opt
        updateState $ updateParsedArgs $ updateFn opt optInfo val

      updateSt_saveOccurrence opt val = updateOptWith saveOccurrence opt val
      updateSt_assertPresent opt = updateOptWith (\opt info _ -> assertPresent opt info) opt ""

      updateSt_inShortOptStack = updateState . updateInShortOptStack
  
  in case pattern of
  (Sequence pats) ->
      assertTopConsumesAll $ foldl (andThen) (return ()) ps 
      where assertTopConsumesAll p = do
              st <- getState
              if inTopLevelSequence st
                then do 
                  updateState $ \st -> st {inTopLevelSequence = False}
                  p <* eof
                else p 
            inner_pats = (\pat -> (pat, infomap)) `map` pats
            ps = (buildOptParser delim) `map` inner_pats
            andThen = \p1 p2 -> do 
              p1
              argDelimIfNotInShortOptStack
              p2
  (OneOf pats) ->
      choice $ (try . makeParser) `map` pats
  (Unordered pats) ->
      choice $ (parseThisThenRest pats) `map` pats
      where parseThisThenRest list pat = try $ do
              makeParser pat
              let rest = list \\ [pat]
              argDelimIfNotInShortOptStack
              makeParser $ Unordered rest
  (Optional pat) ->
        case pat of 
          Unordered ps ->
            optional $ choice $ (parseThisThenRest ps) `map` ps
            where parseThisThenRest list pat = try $ do
                    makeParser pat
                    let rest = list \\ [pat]
                    argDelimIfNotInShortOptStack
                    makeParser $ Optional $ Unordered rest
          _ -> optional $ try $ makeParser pat 
  (Repeated pat) -> do
      case pat of 
        (Optional _) -> (try $ makeParser pat) `sepBy` argDelim
        _            -> (try $ makeParser pat) `sepBy1` argDelim
      return ()
  (Atom pat) -> case pat of 
      o@(ShortOption c) ->
            do  st <- getState
                if inShortOptStack st then return () else char '-' >> return ()
                char c
                updateState $ updateInShortOptStack True
                val <- if expectsVal $ M.findWithDefault (fromSynList []) o infomap 
                  then try $ do 
                    optional $ string "=" <|> argDelim
                    updateState $ updateInShortOptStack False
                    manyTill1 anyChar (lookAhead_ argDelim <|> eof)
                  else do
                    stillInShortStack <- isNotFollowedBy argDelim
                    unless stillInShortStack $ 
                      updateState $ updateInShortOptStack False 
                    return ""
                updateState $ withEachSynonym o $
                              \pa syn info -> saveOccurrence syn info val pa
          <?> humanize o
      o@(LongOption name) ->
            do string "--"
               string name
               val <- if expectsVal $ M.findWithDefault (fromSynList []) o infomap 
                 then do 
                   string "=" <|> argDelim
                   --many (notFollowedBy (string delim) >> anyChar)
                   manyTill1 anyChar (lookAhead_ argDelim <|> eof)
                 else return ""
               updateState $ withEachSynonym o $
                           \pa syn info -> saveOccurrence syn info val pa
               updateState $ updateInShortOptStack False
          <?> humanize o
      o@(AnyOption) ->
            let synlists = nub . map synonyms $ M.elems infomap
                --oneOf syns = OneOf (map Atom syns)
                --synparsers = oneOf `map` synlists
                oneOfSyns = map (\ss -> OneOf (map Atom ss)) synlists
                unorderedSynParser = buildOptParser delim (Unordered oneOfSyns, infomap)
            in  unorderedSynParser 
                <?> humanize o
      o@(Argument name) ->
            do val <- try $ many1 (notFollowedBy argDelim >> anyChar)
               updateSt_saveOccurrence o val
               updateSt_inShortOptStack False
          <?> humanize o
      o@(Command name) ->
            do string name
               updateSt_assertPresent o
               updateSt_inShortOptStack False
          <?> humanize o


-- ** Helpers


-- | converts a parser to return its user-state
--   instead of its return value
returnState :: CharParser u a -> CharParser u u
returnState p = p >> getState

updateInShortOptStack :: Bool -> OptParserState -> OptParserState
updateInShortOptStack b ops = ops {inShortOptStack = b}

updateParsedArgs :: (Arguments -> Arguments) -> OptParserState -> OptParserState
updateParsedArgs f st = st {parsedArgs = f $ parsedArgs st}

saveOccurrence :: Option -> OptionInfo -> String -> Arguments -> Arguments
saveOccurrence opt info newval argmap = M.alter updateCurrentVal opt argmap
    where updateCurrentVal m_oldval = case m_oldval of
            Nothing     -> (newval `updateFrom`) =<< (optInitialValue info opt)
            Just oldval -> newval `updateFrom` oldval 
          updateFrom newval oldval = Just $ case oldval of
            MultiValue vs -> MultiValue $ newval : vs
            Value v       -> Value newval
            NoValue       -> Value newval
            Counted n     -> Counted (n+1)
            Present       -> Present
            NotPresent    -> Present

assertPresent :: Option -> OptionInfo -> Arguments -> Arguments
assertPresent opt info argmap = saveOccurrence opt info "" argmap

withEachSynonym :: Option -> 
                   (Arguments -> Option -> OptionInfo -> Arguments) -> 
                   OptParserState -> 
                   OptParserState
withEachSynonym opt savefn st = 
  let infomap = optInfoMap st
      args = parsedArgs st
      syns = synonyms $ M.findWithDefault (fromSynList []) opt infomap
      -- give the savefn each opt's info, as well
      foldsavefn = \args opt -> 
                    let info = M.findWithDefault (fromSynList []) opt infomap
                    in savefn args opt info 
  in st {parsedArgs = foldl foldsavefn args syns}


optInitialValue :: OptionInfo -> Option -> Maybe ArgValue
optInitialValue info opt = 
  let repeatable = isRepeated info 
  in case opt of
    Command name  -> Just $ if repeatable then Counted 0 else NotPresent
    Argument name -> Just $ if repeatable then MultiValue [] else NoValue
    AnyOption     -> Nothing -- no storable value for [options] shortcut
    _             -> case expectsVal info of 
      True  -> Just $ if repeatable then MultiValue [] else NoValue
      False -> Just $ if repeatable then Counted 0 else NotPresent

optDefaultValue :: OptionInfo -> Option -> Maybe ArgValue
optDefaultValue info opt = 
  let repeatable = isRepeated info
  in case opt of 
    Command name  -> Just $ if repeatable then Counted 0 else NotPresent
    Argument name -> Just $ if repeatable then MultiValue [] else NoValue
    AnyOption     -> Nothing -- no storable value for [options] shortcut
    _               -> case expectsVal info of 
      True  -> case defaultVal info of
        Just dval -> Just $ if repeatable 
                            then MultiValue $ reverse $ words dval
                            else Value dval
        Nothing   -> Just $ if repeatable then MultiValue [] else NoValue
      False -> Just $ if repeatable then Counted 0 else NotPresent


getArguments :: OptFormat -> [String] -> Either ParseError Arguments
getArguments optfmt argv = 
    let (pattern, infomap) = optfmt

        -- delimiter used to flatten argv to parsable String
        delim = "«»" 
        argvString = delim `intercalate` argv

        p = parsedArgs <$> (returnState $ buildOptParser delim optfmt)
        
        patAtoms = atoms pattern
        infoKeys = (\\ [AnyOption]) $ M.keys infomap
        allAtoms = nub $ patAtoms ++ infoKeys
        defaultArgVals = foldl f M.empty allAtoms
            where f argmap atom = M.alter (\_ -> optDefaultValue (infomap M.! atom) atom) atom argmap

        initialState = (fromOptInfoMap infomap)

        e_parsedArgs = runParser p initialState "argv" argvString

        fillMissingDefaults = \pargs -> M.union pargs defaultArgVals

    in fillMissingDefaults <$> e_parsedArgs
