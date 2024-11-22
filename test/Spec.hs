-- test/Spec.hs
module Main where

import Test.Hspec
import Core
import Game

main :: IO ()
main = hspec $ do
  describe "Pruebas de la función match" $ do
    it "match 'posta' 'seria'" $ do
      match "posta" "seria" `shouldBe` [('s', LugarIncorrecto), ('e', NoPertenece), ('r', NoPertenece), ('i', NoPertenece), ('a', Correcto)]

  describe "Pruebas del módulo Game" $ do
    it "Inicializa el juego correctamente" $ do
      let game = initializeGame "HOLA" 5
      secretWord game `shouldBe` "HOLA"
      maxAttempts game `shouldBe` 5
      attemptsMade game `shouldBe` []
      gameOver game `shouldBe` False
      won game `shouldBe` False

    it "Enviar un intento válido y ganar el juego" $ do
      let initialGame = initializeGame "HOLA" 5
      let result = submitAttempt initialGame "HOLA"
      case result of
        Left _ -> expectationFailure "El intento debería ser aceptado."
        Right game -> do
          attemptsMade game `shouldBe` [("HOLA", [('H', Correcto), ('O', Correcto), ('L', Correcto), ('A', Correcto)])]
          gameOver game `shouldBe` True
          won game `shouldBe` True

    it "Enviar un intento inválido por longitud" $ do
      let initialGame = initializeGame "HOLA" 5
      let result = submitAttempt initialGame "HOLO"
      result `shouldBe` Left "Longitud de intento inválida."

    it "Enviar un intento con caracteres inválidos" $ do
      let initialGame = initializeGame "HOLA" 5
      let result = submitAttempt initialGame "H0LA"
      result `shouldBe` Left "Caracteres inválidos en el intento."

    it "Enviar varios intentos y no ganar" $ do
      let initialGame = initializeGame "HOLA" 3
      let result1 = submitAttempt initialGame "HOLA"
      let result2 = case result1 of
                      Left _ -> initialGame
                      Right game -> submitAttempt game "HOLO"
      result2 `shouldBe` Left "El juego ha terminado."
