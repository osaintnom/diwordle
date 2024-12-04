{-# LANGUAGE DeriveGeneric #-}

module Core (Match(..), match) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, encode, decode)

data Match = Correcto | LugarIncorrecto | NoPertenece
  deriving (Eq, Show, Generic)

instance ToJSON Match
instance FromJSON Match

match :: String -> String -> [(Char, Match)]
match objetivo intento = matchesFinal
  where
    -- Inicializamos las listas de matches y caracteres checkeados
    caracteresCheckeadosInit = replicate (length intento) False
    matchesInit = replicate (length intento) NoPertenece

    -- Procesamos primero los casos correctos
    (matchesCorrectos, caracteresCorrectos) =
      foldl checkCorrecto (matchesInit, caracteresCheckeadosInit) (zip3 intento objetivo [0..])

    (matchesCorrectosIncorrectos, _) = 
      foldl (checkNoPertenece objetivo) (matchesCorrectos, caracteresCorrectos) (zip intento [0..])

    matchesFinal = zip intento matchesCorrectosIncorrectos

-- Maneja los casos Correcto
checkCorrecto :: ([Match], [Bool]) -> (Char, Char, Int) -> ([Match], [Bool])
checkCorrecto (matches, checkeados) (letraIntento, letraObjetivo, posicion) =
  if letraIntento == letraObjetivo then
    let checkeadosActualizados = reemplazarValor posicion True checkeados
        matchesActualizados = reemplazarValor posicion Correcto matches
    in (matchesActualizados, checkeadosActualizados)
  else
    let checkeadosActualizados = reemplazarValor posicion False checkeados
        matchesActualizados = reemplazarValor posicion NoPertenece matches
    in (matchesActualizados, checkeadosActualizados)

-- Maneja los casos LugarIncorrecto o NoPertenece
checkNoPertenece :: String -> ([Match], [Bool]) -> (Char, Int) -> ([Match], [Bool])
checkNoPertenece objetivo (matches, checkeados) (letraIntento, posicion)
  | (matches !! posicion) == Correcto = (matches, checkeados)
  | fst (letraEnPalabra letraIntento objetivo checkeados) =
    let checkeadosActualizados = snd (letraEnPalabra letraIntento objetivo checkeados)
        matchesActualizados = reemplazarValor posicion LugarIncorrecto matches
    in (matchesActualizados, checkeadosActualizados)
  | otherwise =
    let checkeadosActualizados = snd (letraEnPalabra letraIntento objetivo checkeados)
        matchesActualizados = reemplazarValor posicion NoPertenece matches
    in (matchesActualizados, checkeadosActualizados)


letraEnPalabra :: Char -> String -> [Bool] -> (Bool, [Bool])
letraEnPalabra letra objetivo checkeados = 
  let aparicion = primeraAparicion letra objetivo checkeados
  in if aparicion == -1 then (False, checkeados) else (True, reemplazarValor aparicion True checkeados)


primeraAparicion :: Char -> String -> [Bool] -> Int
primeraAparicion letra objetivo checkeados
  | null objetivo = -1
  | null posicionesConAparicion = -1
  | otherwise = extractIdx (head posicionesConAparicion)
  where
    posicionesConAparicion = filter (\(char, check, _) -> char == letra && not check) (zip3 objetivo checkeados [0..])

    extractIdx :: (Char, Bool, Int) -> Int
    extractIdx (_, _, idx) = idx


reemplazarValor :: Int -> a -> [a] -> [a]
reemplazarValor i x xs = take i xs ++ [x] ++ drop (i + 1) xs

