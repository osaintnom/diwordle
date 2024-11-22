module Core (Match(..), match) where
import TinyApp.Interactive

data Match = Correcto | LugarIncorrecto | NoPertenece
  deriving (Eq, Show)

-- Función principal que obtiene la lista final con los resultados.
match :: String -> String -> [(Char, Match)]
match objetivo intento = 
  let originalList = checkSiEsCorrecto objetivo (checkSiEsta objetivo intento)
  in checkDuplicados objetivo intento originalList originalList

-- Verifica si una letra está en el objetivo en cualquier posición
checkSiEsta :: String -> String -> [(Char, Match)] 
checkSiEsta _ "" = []
checkSiEsta objetivo (int:intento)
  | int `elem` objetivo = (int, LugarIncorrecto) : checkSiEsta objetivo intento
  | otherwise = (int, NoPertenece) : checkSiEsta objetivo intento

-- Marca las letras correctas (en la posición correcta)
checkSiEsCorrecto :: String -> [(Char, Match)] -> [(Char, Match)]
checkSiEsCorrecto _ [] = []
checkSiEsCorrecto (obj:objetivo) ((c, m):xs) = 
  if c == obj
    then (c, Correcto) : checkSiEsCorrecto objetivo xs
    else (c, m) : checkSiEsCorrecto objetivo xs

-- Ajusta las marcas, cambiando LugarIncorrecto a NoPertenece si es necesario
checkDuplicados :: String -> String -> [(Char, Match)] -> [(Char, Match)] -> [(Char, Match)]
checkDuplicados _ _ _ [] = []
checkDuplicados objetivo intento originalList ((c, m):xs) =
  let countLugarInc = countLugarIncorrectoCorrecto c originalList
      countInTarget = countChar c objetivo
      headless_originalList = tail originalList
  in if m == LugarIncorrecto && countLugarInc > countInTarget
     then (c, NoPertenece) : checkDuplicados objetivo intento headless_originalList xs
     else (c, m) : checkDuplicados objetivo intento originalList xs

-- Cuenta cuántas veces aparece un carácter en un string
countChar :: Char -> String -> Int
countChar c str = length (filter (== c) str)

-- Cuenta cuántas veces una letra aparece marcada como LugarIncorrecto o Correcto
countLugarIncorrectoCorrecto :: Char -> [(Char, Match)] -> Int
countLugarIncorrectoCorrecto _ [] = 0
countLugarIncorrectoCorrecto c ((char, m):xs) = 
  if c == char && (m == LugarIncorrecto || m == Correcto)
    then 1 + countLugarIncorrectoCorrecto c xs 
    else countLugarIncorrectoCorrecto c xs


-- [(s,LugarIncorrecto),(a,LugarIncorrecto),(v,NoPertenece),(i,NoPertenece),(a,Correcto)]
-- Deberia ser
-- [(s,LugarIncorrecto),(a,NoPertenece),(v,NoPertenece),(i,NoPertenece),(a,Correcto)]