{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DeriveGeneric #-}
module CLI(main) where

import Game
import Core (Match(..))
import TinyApp.Interactive (runInteractive', Sandbox(..), Key(..), ContinueExit(..), Event (Key))
import Data.Char (toUpper)
import GHC.Generics (Generic)
import System.IO.Error(catchIOError)
import System.Random.Stateful

data Diccionario = Diccionario {
    palabras :: [String]
} deriving (Generic, Show)


data State = State {
    juego          :: Juego,           -- ^ Estado actual del juego
    entradaUsuario :: String,          -- ^ Entrada actual del usuario
    estadoIntento  :: Maybe ResultadoIntento, -- ^ Estado del intento actual
    mensajeFinal   :: Maybe String,     -- ^ Mensaje de finalización del juego
    diccionario    :: Diccionario
} deriving (Show)

main :: IO ()
main = do
    archivo <- catchIOError (readFile "diccionario.txt") (\_ -> pure "")
    if null archivo then
      putStrLn "Error al cargar el archivo"
    else do
      let nuevoDiccionario = Diccionario {palabras = lines archivo}
      estadoInicial <- cargarArchivo nuevoDiccionario
      s <- runInteractive' (wordle estadoInicial)
      case mensajeFinal s of
          Just mensaje -> putStrLn mensaje
          Nothing -> putStrLn "Gracias por jugar!"

{- Leer archivo e iniciar juego con palabra random -}
cargarArchivo :: Diccionario -> IO State
cargarArchivo dicc = do
    palabra <- obtenerPalabraRandom dicc
    let nuevoJuego = iniciarJuego palabra 5
    return $ State nuevoJuego "" Nothing Nothing dicc

{- Obtiene una palabra random del diccionario -}
obtenerPalabraRandom :: Diccionario -> IO String
obtenerPalabraRandom (Diccionario lista) = do
    indice <- randomRIO (0, length lista - 1)
    return (lista !! indice)

-- palabraEnDiccionario :: Diccionario -> String -> Bool

{- Sandbox -}
wordle :: State -> Sandbox State
wordle primerEstado =
    Sandbox {
        initialize = primerEstado,
        update = \(Key key _) s ->
        if juegoFinalizado (juego s)
          then (s, Exit)
        else
        case key of
          KEsc -> (s, Exit)
          KEnter ->
            let (resultado, nuevoJuego) = enviarIntento (juego s) (entradaUsuario s) (palabras (diccionario s))
              in case resultado of
                  Valido ->
                    if juegoFinalizado nuevoJuego
                      then
                        let mensaje = if ganoJuego nuevoJuego
                                        then "Ganaste!"
                                        else "Perdiste!"
                         in (State nuevoJuego "" (Just Valido) (Just mensaje) (diccionario s), Continue)
                      else
                        (State nuevoJuego "" (Just Valido) Nothing (diccionario s), Continue)
                  LargoInvalido -> (s {estadoIntento = Just LargoInvalido}, Continue)
                  PalabraInvalida -> (s {estadoIntento = Just PalabraInvalida}, Continue)
                  IntentoYaRealizado -> (s {estadoIntento = Just IntentoYaRealizado}, Continue)
                  PalabraNoDiccionario -> (s {estadoIntento = Just PalabraNoDiccionario}, Continue)

          KChar c -> if length (entradaUsuario s) < largoPalabraSecreta (juego s) then (s {entradaUsuario = entradaUsuario s <> [toUpper c]}, Continue) else (s, Continue)
          KBS -> if length (entradaUsuario s) >0 then (s {entradaUsuario = init (entradaUsuario s)}, Continue) else (s, Continue)
          _ -> (s, Continue),

        render = \s ->  showJuego s <>
                        mensajeOut (estadoIntento s) <> "\n" <>
                        if juegoFinalizado (juego s) then "Juego finalizado. La palabra es: " <> obtenerPalabraSecreta (juego s) else " " <> "\n"
    }

{- DISPLAY HELPERS -}
{- Devuelve el string del juego -}
showJuego :: State -> String
showJuego s =
  let j = juego s
      largoPalabra = largoPalabraSecreta j
  in
    if juegoFinalizado j then 
    concatMap showIntento (obtenerIntentos j)
    <> concatMap showVacio (replicate (intentosRestantes j ) largoPalabra)
    <> showSeparador largoPalabra
    else
        concatMap showIntento (obtenerIntentos j)
    <> showSeparador largoPalabra <> showEntrada (entradaUsuario s) largoPalabra
    <> concatMap showVacio (replicate (intentosRestantes j -1) largoPalabra)
    <> showSeparador largoPalabra

{- Devuelve el string de la palabra que escribe el jugador -}
showEntrada :: String -> Int -> String
showEntrada entrada x = 
  concatMap (\c -> "|" <> showCasilla (Just c) ansiBgGrayColor) entrada 
  <> concat (replicate (x - length entrada) ("|" <> showCasilla (Just ' ') ansiBgGrayColor)) 
  <> "| \n"

{- Devuelve el string de una fila vacía -}
showVacio :: Int -> String
showVacio n =
  showSeparador n
  <> concat (replicate n ("|" <> showCasilla (Just ' ') ansiBgGrayColor)) 
  <> "|\n"

{- Devuelve el string de un intento (fila) -}
showIntento :: (String, [(Char, Match)]) -> String
showIntento (_, matches) =
  showSeparador (length matches)
  <> concatMap (\(c, m) -> "|" <> showCasilla (Just c) (selectColor m)) matches
  <> "|\n"

{- Devuelve el string de un separador de casillas (len n) -}
showSeparador :: Int -> String
showSeparador n = concat (replicate n "+---") <> "+\n"

{- Devuelve el string de una casilla (len 3) con un caracter y un color de fondo -}
showCasilla :: Maybe Char -> Color -> String
showCasilla Nothing _ = ansiBgGrayColor <> "   " <> ansiResetColor
showCasilla (Just c) color = color <> " " <> [c] <> " " <> ansiResetColor

{- Selecciona el color de fondo de la casilla según el match -}
selectColor :: Match -> String
selectColor Correcto = ansiBgGreenColor
selectColor LugarIncorrecto = ansiBgYellowColor
selectColor NoPertenece = ansiBgRedColor

{- ANSI ESCAPE COLORS -}
type Color = String
ansiResetColor, ansiBgYellowColor, ansiBgGreenColor, ansiBgRedColor, ansiBgGrayColor :: Color
ansiResetColor = "\ESC[39m\ESC[49m"
ansiBgYellowColor = "\ESC[103m"
ansiBgGreenColor = "\ESC[42m"
ansiBgRedColor = "\ESC[41m"
ansiBgGrayColor = "\ESC[100m"

{- Devuelve el mensaje de error según el resultado del intento -}
mensajeOut :: Maybe ResultadoIntento -> String
mensajeOut Nothing = ""
mensajeOut (Just LargoInvalido) = "Error: La palabra ingresada no tiene el largo correcto\n"
mensajeOut (Just PalabraInvalida) = "Error: La palabra ingresada no es valida\n"
mensajeOut (Just IntentoYaRealizado) = "Error: La palabra ya fue ingresada\n"
mensajeOut (Just PalabraNoDiccionario) = "Error: La palabra no pertenece al diccionario\n"
mensajeOut (Just Valido) = ""



