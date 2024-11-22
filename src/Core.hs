-- src/Core.hs
module Core (Match(..), match) where
import TinyApp.Interactive

data Match = Correcto | LugarIncorrecto | NoPertenece
  deriving (Eq, Show)

match :: String -> String -> [(Char, Match)]
match objetivo intento = checkSiEsCorrecto objetivo (checkSiEsta objetivo intento)

checkSiEsta :: String -> String -> [(Char, Match)] 
checkSiEsta _ "" = []
checkSiEsta objetivo (int:intento)
  | int `elem` objetivo = (int, LugarIncorrecto) : (checkSiEsta objetivo intento)
  | otherwise = (int, NoPertenece) : (checkSiEsta objetivo intento)




checkSiEsCorrecto :: String -> String -> [(Char, Match)] 
checkSiEsCorrecto _ [] = []
checkSiEsCorrecto (obj:objetivo) ((c, m):xs) = 
  if c == obj
    then (c, Correcto) : checkSiEsCorrecto objetivo xs
    else (c, m) : checkSiEsCorrecto objetivo xs




 a s a d o              a o a s o 

 t f t f t           s -> o 
                     d -> s   










checkSiEsCorrecto :: String -> [(Char, Match)] -> [(Char, Match)]
checkSiEsCorrecto _ [] = []
checkSiEsCorrecto (obj:objetivo) ((c, m):xs) = 
  if c == obj
    then (c, Correcto) : checkSiEsCorrecto objetivo xs
    else (c, m) : checkSiEsCorrecto objetivo xs
-- >>> match "posta" "serio"
