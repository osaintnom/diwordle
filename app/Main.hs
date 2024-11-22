module Main where

import Core (match)

main :: IO ()
main = do
  let resultado = match "posta" "assas"
  print resultado


