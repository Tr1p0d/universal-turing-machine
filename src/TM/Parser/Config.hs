module TM.Parser.Config
    (withArgumentParser)
  where

import Data.Monoid
import Options.Applicative

import TM.Types.Config

withArgumentParser :: (Config -> IO ()) -> IO ()
withArgumentParser f = execParser argumentParser >>= f

argumentParser = info (parseConfig <**> helper)
      ( fullDesc
     <> progDesc "Universal turin machine interpret"
     <> header "UTMi" )

parseConfig :: Parser Config
parseConfig = Config
    <$> parseAction
    <*> argument str (metavar "TM")

parseAction :: Parser CAction
parseAction = dump <|> simulate
  where
    dump = flag' Dump (short 'i' <> help "Print TM")
    simulate = flag' Simulate (short 's' <> help "Simulate TM")
