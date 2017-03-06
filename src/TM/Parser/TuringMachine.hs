module TM.Parser.TuringMachine
  where

import Text.ParserCombinators.ReadP
import TM.Types.TuringMachine


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
    newLine
    newLine
    return $ TMachine states transitions initial final

newLine = char '\n'

parseState = many1 $ satisfy (/= ',')

parseStates = commaSep parseState

parseTransitions :: ReadP [Transition]
parseTransitions = newLineSep parseTransition
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

newLineSep :: ReadP a ->  ReadP [a]
newLineSep = sep '\n'

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
