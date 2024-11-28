module Core (Match(..), match) where

data Match = Correcto | LugarIncorrecto | NoPertenece
  deriving (Eq, Show)


match :: String -> String -> [(Char, Match)]
match objetivo intento = 
  let statuses = []





{-| Dada una palabra objetivo y un intento, devuelve una lista de tuplas
    donde el primer elemento es una letra del intento y el segundo es
    un valor de tipo Match que indica si la letra es correcta y está en
    la posición correcta, si es correcta pero está en la posición incorrecta
    o si no pertenece a la palabra objetivo.
    
    >>> match "posta" "seria"
    [('s',LugarIncorrecto),('e',NoPertenece),('r',NoPertenece),('i',NoPertenece),('a',Correcto)]
-}
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
checkSiEsCorrecto "" _ = []
checkSiEsCorrecto (obj:objetivo) ((c, m):xs) = 
  if c == obj
    then (c, Correcto) : checkSiEsCorrecto objetivo xs
    else (c, m) : checkSiEsCorrecto objetivo xs

-- Ajusta las marcas, cambiando LugarIncorrecto a NoPertenece si es necesario
checkDuplicados :: String -> String -> [(Char, Match)] -> [(Char, Match)] -> [(Char, Match)]
checkDuplicados _ _ _ [] = []
checkDuplicados objetivo intento originalList lista =
  let (c, m) = last lista
      xs = init lista
      countLugarInc = countLugarIncorrectoCorrecto c originalList
      countInTarget = countChar c objetivo
      headless_originalList = init originalList
  in if m == LugarIncorrecto && countLugarInc > countInTarget
     then  checkDuplicados objetivo intento headless_originalList xs ++ [(c, NoPertenece)]
     else  checkDuplicados objetivo intento originalList xs ++ [(c, m)]

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
