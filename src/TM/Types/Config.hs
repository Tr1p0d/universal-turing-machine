module TM.Types.Config
    ( CAction(Dump, Simulate)
    , Config(Config)
    , config
    , machine
    )
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
