-- | Main entry point to the application.
{-# LANGUAGE RecordWildCards #-}

module Main where
import Data.Monoid
import Options.Applicative
import System.Log.Handler.Syslog
import System.Log.Logger

data Options = Options
  { broker :: String
  , debug :: Bool
  , namespace :: String } deriving Show

helpfulParser :: ParserInfo Options
helpfulParser = info (helper <*> optionsParser) fullDesc 
optionsParser :: Parser Options
optionsParser = Options <$> parseBroker
                                    <*> parseDebug
                                    <*> parseNameSpace
  where
    parseBroker = strOption $
           long "broker"
        <> short 'b'
        <> metavar "BROKER"
        <> value "localhost"
        <> showDefault
        <> help "Vault broker host name or IP address"

    parseDebug = switch $
           long "debug"
        <> short 'd'
        <> help "Set log level to DEBUG"

    parseNameSpace = argument str (metavar "NAMESPACE")

main :: IO ()
main = do
    Options{..} <- execParser helpfulParser 
    print $ Options broker debug namespace
