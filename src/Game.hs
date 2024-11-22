-- src/Game.hs
module Game (
    GameState,
    initializeGame,
    submitAttempt,
    getAttempts,
    isGameOver,
    didWin,
    secretWordLength,
    remainingAttempts,
    totalAttempts
) where

import Core (Match(..), match)
import Data.Char (toUpper)

data GameState = GameState {
    secretWord     :: String,                   -- La palabra secreta a adivinar
    maxAttempts    :: Int,                      -- Número máximo de intentos permitidos
    attemptsMade   :: [(String, [(Char, Match)])],  -- Lista de intentos realizados y sus matches
    gameOver       :: Bool,                     -- Indica si el juego ha terminado
    won            :: Bool                      -- Indica si el juego se ha ganado
} deriving (Show)

-- Inicializar el juego con la palabra secreta y el número de intentos
initializeGame :: String -> Int -> GameState
initializeGame word maxAtts = GameState {
    secretWord = map toUpper word,
    maxAttempts = maxAtts,
    attemptsMade = [],
    gameOver = False,
    won = False
}

-- Enviar un intento al juego
submitAttempt :: GameState -> String -> Either String GameState
submitAttempt state attempt
  | gameOver state = Left "El juego ha terminado."
  | length attempt /= length (secretWord state) = Left "Longitud de intento inválida."
  | not (all (`elem` ['A'..'Z']) intentoUpper) = Left "Caracteres inválidos en el intento."
  | otherwise =
      let matches = match (secretWord state) intentoUpper
          newAttempts = attemptsMade state ++ [(intentoUpper, matches)]
          wonGame = all ((== Correcto) . snd) matches
          gameEnded = wonGame || length newAttempts >= maxAttempts state
          newState = state {
            attemptsMade = newAttempts,
            gameOver = gameEnded,
            won = wonGame
          }
      in Right newState
  where
    intentoUpper = map toUpper attempt

-- Obtener todos los intentos realizados junto con sus matches
getAttempts :: GameState -> [(String, [(Char, Match)])]
getAttempts = attemptsMade
-- Verificar si el juego ha terminado
isGameOver :: GameState -> Bool
isGameOver = gameOver

-- Verificar si el juego ha sido ganado
didWin :: GameState -> Bool
didWin = won

-- Obtener la longitud de la palabra secreta
secretWordLength :: GameState -> Int
secretWordLength = length . secretWord

-- Obtener la cantidad de intentos restantes
remainingAttempts :: GameState -> Int
remainingAttempts state = maxAttempts state - length (attemptsMade state)

-- Obtener el total de intentos permitidos
totalAttempts :: GameState -> Int
totalAttempts = maxAttempts
