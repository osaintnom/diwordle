module Main where

import Core (Match(..), match)

main :: IO ()
main = do
  let resultado = match "posta" "savia"
  print resultado


