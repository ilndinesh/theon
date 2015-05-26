{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Theon
import Options.Applicative



options :: Parser Options
options = Options
  <$> strOption (long "journal"
    <> short 'j'
    <> metavar "FILE"
    <> help "Path to journal file (default: theon.db)"
    <> value "theon.db")
  <*> option auto (long "port"
    <> short 'p'
    <> metavar "NUM"
    <> help "Port to serve application (default: 3000)"
    <> value 3000)


mode :: Parser Mode
mode = 
  flag' VersionMode
    (long "version"
      <> hidden
      <> short 'v'
      <> help "Show the application version")
  <|> (NormalMode <$> options)


main :: IO ()
main = execParser opts >>= main'
  where
    opts = info (helper <*> mode)
      (fullDesc <> header "theon - Kafka event spooler")

    main' :: Mode -> IO ()
    main' VersionMode = versionMode
    main' (NormalMode opts) = normalMode opts
