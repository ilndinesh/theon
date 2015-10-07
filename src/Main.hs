{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Theon
import Options.Applicative


main :: IO ()
main = execParser opts >>= main'
  where
    opts = info (helper <*> mode)
      (fullDesc <> header "theon - A simple HTTP-to-Kafka relay built for speed")

    main' :: Mode -> IO ()
    main' VersionMode = versionMode
    main' (NormalMode opts) = normalMode opts


mode :: Parser Mode
mode =
  flag' VersionMode
    (long "version"
      <> hidden
      <> short 'v'
      <> help "Show the application version")
  <|> (NormalMode <$> options)


options :: Parser Options
options = Options
  <$> strOption (long "broker"
    <> short 'b'
    <> metavar "HOST_PORT"
    <> help "Kafka broker address(es) (default: localhost:9092)"
    <> value "localhost:9092")
  <*> option auto (long "port"
    <> short 'p'
    <> metavar "NUM"
    <> help "Port to serve application (default: 3000)"
    <> value 3000)
  <*> switch (long "auth"
    <> short 'a'
    <> help "Enable HTTP Basic Auth (default: false)")
  <*> strOption (long "user"
    <> short 'u'
    <> metavar "USERNAME"
    <> help "Username for HTTP Basic Auth (default: theon)"
    <> value "theon")
  <*> strOption (long "pass"
    <> short 'w'
    <> metavar "PASSWORD"
    <> help "Password for HTTP Basic Auth (default: greyjoy)"
    <> value "greyjoy")