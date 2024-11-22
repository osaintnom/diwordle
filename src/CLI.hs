module CLI(main) where

import Game

main :: IO ()
main = putStrLn "Hello, Haskell!"

{-
-como hacer CLI-
1.      Obtener por consola palabra secreta y crear juego
2.      Mientras juego no este finalizado:
2.1.        print nuevo estado
2.2.        input = RecibirInput()
2.3.        error o estado = enviarIntento input estado
2.4.        si error:  (error := {palabra invalida, largo invalido})
2.4.1           volver a (2.2)
2.4.2       else si estado:
2.4.2.1         si juegoFinalizado estado:
2.4.2.1.1           devolver si gano y cant intentos 
2.4.2.2         else: 
                    volver a (2.2)
3.      Mostrar resultados de cuantas veces jugo?

todos se actualiza con conceptos visuales

1 inicia en cli un juego completo --> 2 es el loop jugando hasta que termine --> 3 es el estado final cuando el jeugo ya termino y muestra las stats
-}