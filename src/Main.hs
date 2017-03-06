{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Environment (getArgs)
import System.IO
import Options.Applicative (execParser)

import TM.Types.Config
import TM.Types.TuringMachine
import TM.Parser.Config
import TM.Parser.TuringMachine

import Debug.Trace

main :: IO ()
main = do
    config <- execParser argumentParser
    tape <- mkTape <$> hGetContents stdin
    result <- handleTM config tape <$> readFile (machine config)
    either print print' result
  where
    handleTM Config{..} tape tmPath = do
        tm <- parseTM tmPath
        case config of
            Dump -> Right [show tm]
            Simulate -> simulate tape tm

    print' = mapM_ putStrLn

simulate :: Tape -> TMachine -> Either TError [String]
simulate t m = step (start m) t m []

step :: TState -> Tape -> TMachine -> [String] -> Either TError [String]
step currentState t m@TMachine{..} output
    | currentState == end = Right (show t:output)
    | otherwise = do
        (newState, newTape, newOutput) <- applyRule
        step newState newTape m (newOutput:output)
  where
    applyRule = case filter (mayTransition t) trans of
        [] -> Left $ NoTransition
        (Transition{..}:_) -> do
            newTape <- moveTape t action
            Right (to, newTape, show t)

    mayTransition (Tape s _ _) Transition{..} =
        from == currentState
        && s == symbol

moveTape :: Tape -> TAction -> Either TError Tape
moveTape (Tape s l r)= \case
    TLeft -> case l of
        [] -> Left $ EndOfTape
        (l:ls) -> Right $ Tape l ls (s:r)

    TRight -> case r of
        [] -> Right $ Tape '$' (s:l) []
        (r:rs) -> Right $ Tape r (s:l) rs

    TWrite a -> Right $ Tape a l r
