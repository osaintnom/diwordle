{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified CLI as CLI
import Parser (parseConfig)

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  config <- parseConfig args
  CLI.main config
