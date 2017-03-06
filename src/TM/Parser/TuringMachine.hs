module TM.Parser.TuringMachine
    (parseTM, tmParser)
  where

import Text.ParserCombinators.ReadP
    ( ReadP
    , char
    , eof
    , get
    , many
    , many1
    , readP_to_S
    , satisfy
    )
import TM.Types.TuringMachine
    ( TError(CannotParse)
    , TMachine(TMachine)
    , Transition(Transition)
    , TAction(TLeft, TRight, TWrite)
    )


parseTM :: String -> Either TError TMachine
parseTM s = case readP_to_S tmParser s of
    [(tm, _)] -> Right tm
    _ -> Left CannotParse

tmParser :: ReadP TMachine
tmParser = do
    states <- parseStates
    newLine
    initial <- parseState
    newLine
    final <- parseState
    newLine
    transitions <- parseTransitions
    eof
    return $ TMachine states transitions initial final

newLine = char '\n'

parseState = many1 $ satisfy (/= ',')

parseStates = commaSep parseState

parseTransitions :: ReadP [Transition]
parseTransitions = many1 $ do
    t <- parseTransition
    newLine
    return t
  where
    parseComma = char ','
    parseTransition = do
        from <- parseState
        parseComma
        symbol <- get
        parseComma
        to <- parseState
        parseComma
        action <- parseAction
        return $ Transition from to symbol action

parseAction = do
    a <- get
    return $ case a of
        '>' -> TRight
        '<' -> TLeft
        w -> TWrite w

commaSep :: ReadP a -> ReadP [a]
commaSep = sep ','

sep :: Char -> ReadP a ->  ReadP [a]
sep c p = do
    es <- many p'
    e <- p
    return (es ++ [e])
  where
    p' = do
        r <- p
        char c
        return r
