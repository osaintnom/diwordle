{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module CLI(main) where

import Game
import Core (match, Match(..))
import TinyApp.Interactive
import Data.Char (toUpper)


data State = State {
    juego          :: Juego,           -- ^ Estado actual del juego
    entradaUsuario :: String,          -- ^ Entrada actual del usuario
    estadoIntento  :: Maybe ResultadoIntento, -- ^ Estado del intento actual
    mensajeFinal   :: Maybe String     -- ^ Mensaje de finalización del juego
} deriving (Show)


main :: IO ()
main = do
    palabra <- getLine
    let nuevoJuego = iniciarJuego palabra 5
    let estado_inicial = State nuevoJuego "" Nothing Nothing
    putStrLn "Bienvenido a Wordle!"
    s <- runInteractive' (wordle estado_inicial)
    case mensajeFinal s of
        Just mensaje -> putStrLn mensaje
        Nothing -> putStrLn "Gracias por jugar!"

wordle :: State -> Sandbox State
wordle primerEstado =
    Sandbox {
        initialize = primerEstado,
        -- juego = vacio?
        -- entradaUsuario = ""
        -- mensajeFinal = Nothing
        update = \(Key key _) s ->
        if juegoFinalizado (juego s)
          then (s, Exit)
        else
        case key of
          KEsc -> (s, Exit)
          KEnter ->
            let (resultado, nuevoJuego) = enviarIntento (juego s) (entradaUsuario s)
             in case resultado of
                  Valido ->
                    if juegoFinalizado nuevoJuego
                      then
                        let mensaje = if ganoJuego nuevoJuego
                                        then "Ganaste!"
                                        else "Perdiste!"
                         in (State nuevoJuego "" (Just Valido) (Just mensaje), Continue)
                      else
                        (State nuevoJuego "" (Just Valido) Nothing, Continue)
                  LargoInvalido -> (s {estadoIntento = Just LargoInvalido}, Continue)
                  PalabraInvalida -> (s {estadoIntento = Just PalabraInvalida}, Continue)
                  IntentoYaRealizado -> (s {estadoIntento = Just IntentoYaRealizado}, Continue)

          KChar c -> if length (entradaUsuario s) < largoPalabraSecreta (juego s) then (s {entradaUsuario = entradaUsuario s <> [c]}, Continue) else (s, Continue)
          KBS -> (s {entradaUsuario = init (entradaUsuario s)}, Continue)
          _ -> (s, Continue),

        render = (\s ->  showJuego (juego s) <> 
                        if intentosRestantes (juego s) > 0 then showEntrada (entradaUsuario s) (largoPalabraSecreta (juego s)) else "" <> "\n" <> 
                        mensajeOut (estadoIntento s)<>
                        -- juego termiando? xq no imprime
                        if juegoFinalizado (juego s) then "Juego finalizado" else "")
    }
-- CORREGIR MENSAJEOUT Y VER PORQUE NO ANDA
mensajeOut :: Maybe ResultadoIntento -> String
mensajeOut Nothing = "adasd"
mensajeOut (Just LargoInvalido) = "Error: La palabra ingresada no tiene el largo correcto\n"
mensajeOut (Just PalabraInvalida) = "Error: La palabra ingresada no es valida\n"
mensajeOut (Just IntentoYaRealizado) = "Error: La palabra ingresada ya fue ingresada\n"
mensajeOut (Just Valido) = "todo bien"

showEntrada :: String -> Int -> String
showEntrada entrada x = concatMap (\c -> " | " <> [toUpper c] <> " | ") entrada <> concat (replicate (x - length entrada) " | _ | ") <> "\n"


showJuego :: Juego -> String
showJuego j =
    concatMap showIntento (obtenerIntentos j)
    <> concatMap showVacio (replicate (intentosRestantes j - 1) (largoPalabraSecreta j))

showVacio :: Int -> String
showVacio n = concat (replicate n " | _ | ") <> "\n\n"

showIntento :: (String, [(Char, Match)]) -> String
showIntento (_, matches) =
    " " <> concatMap (\m -> showMatch m <> "  ") matches <> "\n\n"


showMatch :: (Char, Match) -> String
showMatch (c, m) =
    case m of
        Correcto -> "| " <> ansiBgGreenColor <> [toUpper c] <> ansiResetColor <> " |"
        LugarIncorrecto -> "| " <> ansiBgYellowColor <> [toUpper c] <> ansiResetColor <> " |"
        NoPertenece -> "| " <> ansiBgRedColor <> [toUpper c] <> ansiResetColor <> " |"

ansiResetColor, ansiBgYellowColor, ansiBgGreenColor, ansiBgRedColor :: String
ansiResetColor = "\ESC[39m\ESC[49m"
ansiBgYellowColor = "\ESC[103m"
ansiBgGreenColor = "\ESC[42m"
ansiBgRedColor = "\ESC[41m"

{- Ejemplo de como seria un output
main :: IO ()
main = putStrLn $ ansiResetColor <> "|" <> ansiBgYellowColor <> " A " <> ansiResetColor <> "|" <> ansiBgGreenColor <> " V " <> ansiResetColor <> "|" <> ansiBgRedColor <> " R " <> ansiResetColor <> "|"
-}


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

1 inicia en cli un juego completo --> 
    2 es el loop jugando hasta que termine --> 
        3 es el estado final cuando el jeugo ya termino y muestra las stats
-}



