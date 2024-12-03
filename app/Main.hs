{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified CLI
import Options.Applicative
import Data.Semigroup ((<>))

-- Parser para los modos de la aplicación
modoParser :: Parser CLI.Modo
modoParser =
  flag' CLI.Random
    ( long "random"
    <> help "Selecciona el modo random (palabra aleatoria)" )
  <|> CLI.Daily <$> optional
    ( strOption
      ( long "daily"
      <> metavar "DATE"
      <> help "Modo diario, con la fecha actual o una específica (formato YYYY-MM-DD)" )
    )
  <|> CLI.FixedWord
    <$> strOption
      ( long "palabra"
      <> metavar "WORD"
      <> help "Modo con palabra fija a usar" )
  <|> pure (CLI.Daily Nothing) -- Modo daily con la fecha actual (por defecto)

-- ParserInfo para la aplicación
opts :: ParserInfo CLI.Modo
opts = info (modoParser <**> helper)
      ( fullDesc
     <> progDesc "Juego DIWordle con opciones de modo daily, random o palabra fija"
     <> header "diwordle - Adivina la palabra del día" )

main :: IO ()
main = do
  modo <- execParser opts
  CLI.main modo
