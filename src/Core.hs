-- src/Core.hs
module Core (Match(..), match) where
import TinyApp.Interactive

data Match = Correcto | LugarIncorrecto | NoPertenece
  deriving (Eq, Show)

match :: String -> String -> [(Char, Match)]
match objetivo intento = checkDuplicados objetivo intento (checkSiEsCorrecto objetivo (checkSiEsta objetivo intento))

checkSiEsta :: String -> String -> [(Char, Match)] 
checkSiEsta _ "" = []
checkSiEsta objetivo (int:intento)
  | int `elem` objetivo = (int, LugarIncorrecto) : (checkSiEsta objetivo intento)
  | otherwise = (int, NoPertenece) : (checkSiEsta objetivo intento)

checkSiEsCorrecto :: String -> [(Char, Match)] -> [(Char, Match)]
checkSiEsCorrecto _ [] = []
checkSiEsCorrecto (obj:objetivo) ((c, m):xs) = 
  if c == obj
    then (c, Correcto) : checkSiEsCorrecto objetivo xs
    else (c, m) : checkSiEsCorrecto objetivo xs
-- >>> match "posta" "savia"

checkDuplicados :: String -> String -> [(Char, Match)] -> [(Char, Match)]
checkDuplicados _ _ [] = []
checkDuplicados objetivo intento ((c, m):xs)
  | m == LugarIncorrecto && (countChar c intento) > (countChar c objetivo) = (c, NoPertenece) : checkDuplicados objetivo intento xs
  | otherwise = (c, m) : checkDuplicados objetivo intento xs 


countChar :: Char -> String -> Int
countChar c str = length (filter (== c) str)


-- [(s,LugarIncorrecto),(a,LugarIncorrecto),(v,NoPertenece),(i,NoPertenece),(a,Correcto)]
-- Deberia ser
-- [(s,LugarIncorrecto),(a,NoPertenece),(v,NoPertenece),(i,NoPertenece),(a,Correcto)]