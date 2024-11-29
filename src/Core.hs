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
      foldl checkCorrecto (matchesInit, caracteresCheckeadosInit) (zip3 intento solucion [0..])

    (matchesCorrectosIncorrectos, _) = 
      foldl (checkNoPertenece solucion) (matchesCorrectos, caracteresCorrectos) (zip intento [0..])
      -- foldl (checkNoPertenece solucion) (matchesCorrectos, caracteresCorrectos) (zip intento [0..])

    matchesFinal = zip intento matchesCorrectosIncorrectos

-- Maneja los casos Correcto
checkCorrecto :: ([Match], [Bool]) -> (Char, Char, Int) -> ([Match], [Bool])
checkCorrecto (matches, checkeados) (letraIntento, letraSolucion, posicion) =
  if letraIntento == letraSolucion then
    let checkeadosActualizados = reemplazarValor posicion True checkeados
        matchesActualizados = reemplazarValor posicion Correcto matches
    in (matchesActualizados, checkeadosActualizados)
  else
    let checkeadosActualizados = reemplazarValor posicion False checkeados
        matchesActualizados = reemplazarValor posicion NoPertenece matches
    in (matchesActualizados, checkeadosActualizados)

-- Maneja los casos LugarIncorrecto o NoPertenece
checkNoPertenece :: String -> ([Match], [Bool]) -> (Char, Int) -> ([Match], [Bool])
checkNoPertenece solucion (matches, checkeados) (letraIntento, posicion)
  | (matches !! posicion) == Correcto = (matches, checkeados)
  | fst (letraEnPalabra letraIntento solucion checkeados) =
    let checkeadosActualizados = snd (letraEnPalabra letraIntento solucion checkeados)
        matchesActualizados = reemplazarValor posicion LugarIncorrecto matches
    in (matchesActualizados, checkeadosActualizados)
  | otherwise =
    let checkeadosActualizados = snd (letraEnPalabra letraIntento solucion checkeados)
        matchesActualizados = reemplazarValor posicion NoPertenece matches
    in (matchesActualizados, checkeadosActualizados)


letraEnPalabra :: Char -> String -> [Bool] -> (Bool, [Bool])
letraEnPalabra letra solucion checkeados = 
  let aparicion = primeraAparicion letra solucion checkeados
  in if aparicion == -1 then (False, checkeados) else (True, reemplazarValor aparicion True checkeados)


primeraAparicion :: Char -> String -> [Bool] -> Int
primeraAparicion letra solucion checkeados
  | null solucion = -1
  | null posicionesConAparicion = -1
  | otherwise = extractIdx (head posicionesConAparicion)
  where
    posicionesConAparicion = filter (\(char, check, _) -> char == letra && not check) (zip3 solucion checkeados [0..])

    extractIdx :: (Char, Bool, Int) -> Int
    extractIdx (_, _, idx) = idx


-- Reemplaza el elemento en un índice específico en una lista
reemplazarValor :: Int -> a -> [a] -> [a]
reemplazarValor i x xs = take i xs ++ [x] ++ drop (i + 1) xs

