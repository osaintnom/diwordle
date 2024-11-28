module Core (Match(..), match) where

data Match = Correcto | LugarIncorrecto | NoPertenece
  deriving (Eq, Show)

match :: String -> String -> [(Char, Match)]
match intento solucion = matchesFinal
  where
    -- Inicializamos las listas de matches y caracteres checkeados
    caracteresCheckeadosInit = replicate (length intento) False
    matchesInit = replicate (length intento) NoPertenece

    -- Procesamos primero los casos correctos
    (matchesCorrectos, caracteresCorrectos) =
      foldl checkCasosCorrectos (matchesInit, caracteresCheckeadosInit) (zip3 intento solucion [0..])

    (matchesCorrectosIncorrectos, _) =
      foldl (checkCasosIncorrectos solucion) (matchesCorrectos, caracteresCorrectos) (zip intento [0..])

    matchesFinal = zip intento matchesCorrectosIncorrectos

-- Maneja los casos Correcto
checkCasosCorrectos :: ([Match], [Bool]) -> (Char, Char, Int) -> ([Match], [Bool])
checkCasosCorrectos (matches, checkeados) (letraIntento, letraSolucion, posicion) =
  if letraIntento == letraSolucion then
    let checkeadosActualizados = reemplazarValor posicion True checkeados
        matchesActualizados = reemplazarValor posicion Correcto matches
    in (matchesActualizados, checkeadosActualizados)
  else
    let checkeadosActualizados = reemplazarValor posicion False checkeados
        matchesActualizados = reemplazarValor posicion NoPertenece matches
    in (matchesActualizados, checkeadosActualizados)

-- Maneja los casos LugarIncorrecto o NoPertenece
checkCasosIncorrectos :: String -> ([Match], [Bool]) -> (Char, Int) -> ([Match], [Bool])
checkCasosIncorrectos solucion (matches, checkeados) (letraIntento, posicion)
  | (matches !! posicion) == Correcto = (matches, checkeados)
  | fst (letraEnPalabra letraIntento solucion checkeados) =
    let checkeadosActualizados = reemplazarValor (snd (letraEnPalabra letraIntento solucion checkeados)) True checkeados
        matchesActualizados = reemplazarValor posicion LugarIncorrecto matches
    in (matchesActualizados, checkeadosActualizados)
  | otherwise =
    let checkeadosActualizados = reemplazarValor (snd (letraEnPalabra letraIntento solucion checkeados)) True checkeados
        matchesActualizados = reemplazarValor posicion NoPertenece matches
    in (matchesActualizados, checkeadosActualizados)


letraEnPalabra :: Char -> String -> [Bool] -> (Bool, Int)
letraEnPalabra letra solucion checkeados = 
  case filter (\x -> solucion !! x == letra && not (checkeados !! x)) [0..length solucion - 1] of
    [] -> (False, -1)
    (posicion:_) -> (True, posicion)

-- Reemplaza el elemento en un índice específico en una lista
reemplazarValor :: Int -> a -> [a] -> [a]
reemplazarValor i x xs = take i xs ++ [x] ++ drop (i + 1) xs
