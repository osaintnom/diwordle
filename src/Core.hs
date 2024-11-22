-- src/Core.hs
module Core (Match(..), match) where

import TinyApp.Interactive
import Data.Char (toUpper)
import Data.List (group, sort)

data Match = Correcto | LugarIncorrecto | NoPertenece
  deriving (Eq, Show)



match :: String -> String -> [(Char, Match)]
match "" "" = []
match objetivo intento = zip intento (map matchChar intento)
  where
    matchChar c
      | c `elem` objetivo = if c `elem` intento then Correcto else LugarIncorrecto
      | otherwise = NoPertenece



-- >>> match "posta" "seria"
