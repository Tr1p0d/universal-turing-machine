{-# LANGUAGE LambdaCase #-}

module TM.Types.TuringMachine
    ( TAction(TLeft, TRight, TWrite)
    , TError(CannotParse, EndOfTape, NoTransition)
    , TMachine(TMachine)
    , TState
    , Tape(Tape)
    , Transition(Transition)
    , action
    , end
    , from
    , mkTape
    , start
    , symbol
    , to
    , trans
    )
  where


type TState = String
type TSymbol = Char

data TAction
    = TLeft
    | TRight
    | TWrite TSymbol
  deriving (Show, Eq)

data Transition = Transition
    { from :: TState
    , to :: TState
    , symbol :: TSymbol
    , action :: TAction
    }
  deriving (Show, Eq)

data TMachine = TMachine
    { states :: [TState]
    , trans :: [Transition]
    , start :: TState
    , end :: TState
    }
  deriving (Show, Eq)

data TError
    = CannotParse
    | EndOfTape
    | NoTransition
  deriving (Show)

data Tape = Tape TSymbol [TSymbol] [TSymbol]
  deriving (Show)

mkTape :: String -> Tape
mkTape s = Tape '$' [] (filter (/= '\n') s)
