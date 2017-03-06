module TM.Types.Config
  where

import TM.Types.TuringMachine


data CAction
    = Simulate
    | Dump
  deriving (Show)

data Config = Config
    { config :: CAction
    , machine :: FilePath
    }
  deriving (Show)
