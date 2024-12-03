{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module CLI (main, Modo(..)) where

import Game
import Core (Match (..))

import Data.Char (toUpper, isAlpha)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import System.Directory (doesFileExist)
import qualified Data.ByteString.Lazy as B
import System.IO.Error (catchIOError)
import System.Random.Stateful
import TinyApp.Interactive (ContinueExit (..), Event (Key), Key (..), Sandbox (..), runInteractive')
import Data.List (nub)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Time

newtype Diccionario = Diccionario
  { palabras :: [String]
  }
  deriving (Generic)

data State = State
  { -- | Estado actual del juego
    juego :: Juego,
    -- | Entrada actual del usuario
    entradaUsuario :: String,
    -- | Estado del intento actual
    estadoIntento :: Maybe ResultadoIntento,
    -- | Mensaje de finalización del juego
    mensajeFinal :: Maybe String,
    -- | Diccionario de palabras
    diccionario :: Diccionario
  }

data Modo = Daily (Maybe String) | Random | FixedWord String deriving Show

main :: Modo -> IO ()
main modo = do
  archivo <- catchIOError (readFile "diccionario.txt") (\_ -> pure "")
  if null archivo
    then
      putStrLn "Error al cargar el archivo"
    else do
      let diccionario = Diccionario {palabras = lines archivo}
      case modo of
        Daily Nothing -> do
          juegoAnterior <- cargarJuego "intentos.json" diccionario
          case juegoAnterior of
            Just juegoAnterior -> do
              let estadoInicial = State (juegoAnterior) "" Nothing Nothing diccionario
              estado <- runInteractive' (wordle estadoInicial)
              guardarIntentos "intentos.json" (obtenerIntentos (juego estado))
              mostrarMensajeFinal estado
            Nothing -> do
              currentDate <- fmap (show . utctDay) getCurrentTime
              estadoInicial <- inicializarConFecha diccionario currentDate 5
              estado <- runInteractive' (wordle estadoInicial)
              guardarIntentos "intentos.json" (obtenerIntentos (juego estado))
              mostrarMensajeFinal estado

        Daily (Just date) -> do
          if fechaValida date
            then do
              estadoInicial <- inicializarConFecha diccionario date 5
              estado <- runInteractive' (wordle estadoInicial)
              mostrarMensajeFinal estado
            else putStrLn "Fecha inválida"

        Random -> do
          estadoInicial <- inicializarRandom diccionario 5
          estado <- runInteractive' (wordle estadoInicial)
          mostrarMensajeFinal estado

        FixedWord p -> do
          let palabra = map toUpper p
          if palabraEnDiccionario diccionario palabra
            then do
              let estadoInicial = State (iniciarJuego palabra 5 (palabraEnDiccionario diccionario)) "" Nothing Nothing diccionario
              estado <- runInteractive' (wordle estadoInicial)
              mostrarMensajeFinal estado
            else putStrLn "La palabra no pertenece al diccionario"

mostrarMensajeFinal :: State -> IO ()
mostrarMensajeFinal s = case mensajeFinal s of
  Just mensaje -> putStrLn mensaje
  Nothing -> putStrLn "Gracias por jugar!"

inicializarConFecha :: Diccionario -> String -> Int -> IO State
inicializarConFecha diccionario fecha maxIntentos = do
  palabra <- obtenerPalabraFecha diccionario fecha
  let nuevoJuego = iniciarJuego palabra maxIntentos (palabraEnDiccionario diccionario)
  return $ State nuevoJuego "" Nothing Nothing diccionario

obtenerPalabraFecha :: Diccionario -> String -> IO String
obtenerPalabraFecha diccionario fecha = do
  let indice = (read (filter (/= '-') fecha) :: Int) `mod` length (palabras diccionario)
  return (palabras diccionario !! indice)

{- Check si la fecha es valida -}
fechaValida :: String -> Bool
fechaValida fecha =
  case iso8601ParseM fecha :: Maybe Day of
    Just _  -> True
    Nothing -> False

{- Iniciar juego con palabra random del diccionario -}
inicializarRandom :: Diccionario -> Int -> IO State
inicializarRandom diccionario maxIntentos= do
  palabra <- obtenerPalabraRandom diccionario
  let nuevoJuego = iniciarJuego palabra maxIntentos (palabraEnDiccionario diccionario)
  return $ State nuevoJuego "" Nothing Nothing diccionario

{- Obtiene una palabra random del diccionario -}
obtenerPalabraRandom :: Diccionario -> IO String
obtenerPalabraRandom (Diccionario lista) = do
  indice <- randomRIO (0, length lista - 1)
  return (lista !! indice)

{- Verifica si una palabra pertenece al diccionario -}
palabraEnDiccionario :: Diccionario -> String -> Bool
palabraEnDiccionario diccionario palabra = palabra `elem` palabras diccionario

{- Sandbox -}
wordle :: State -> Sandbox State
wordle primerEstado =
  Sandbox
    { initialize = primerEstado,
      update = actualizarEstado,
      render = renderJuego
    }

{- Actualiza el estado del juego -}
actualizarEstado :: Event -> State -> (State, ContinueExit)
actualizarEstado (Key key _) s =
  if juegoFinalizado (juego s)
    then (s, Exit)
  else case key of
      -- Salir del juego
      KEsc -> (s, Exit)

      -- Enviar intento
      KEnter ->
        let (resultado, nuevoJuego) = enviarIntento (juego s) (entradaUsuario s)
          in case resultado of
              Valido ->
                if juegoFinalizado nuevoJuego
                  then
                    let mensaje =
                          if ganoJuego nuevoJuego
                            then "Ganaste!"
                            else "Perdiste!"
                      in (State nuevoJuego "" (Just Valido) (Just mensaje) (diccionario s), Continue)
                  else
                    (State nuevoJuego "" (Just Valido) Nothing (diccionario s), Continue)
              LargoInvalido -> (s {estadoIntento = Just LargoInvalido}, Continue)
              PalabraInvalida -> (s {estadoIntento = Just PalabraInvalida}, Continue)
              IntentoYaRealizado -> (s {estadoIntento = Just IntentoYaRealizado}, Continue)
              PalabraNoDiccionario -> (s {estadoIntento = Just PalabraNoDiccionario}, Continue)

      -- Ingresar letra
      KChar c -> if isAlpha c then
                  if length (entradaUsuario s) < largoPalabraSecreta (juego s)
                    then (s {entradaUsuario = entradaUsuario s <> [toUpper c]}, Continue)
                  else (s, Continue)
                else (s {estadoIntento = Just CaracterInvalido}, Continue)

      -- Borrar letra
      KBS -> if length (entradaUsuario s) > 0 then (s {entradaUsuario = init (entradaUsuario s)}, Continue) else (s, Continue)
      _ -> (s, Continue)

{- Renderiza el juego -}
renderJuego :: State -> String
renderJuego s =
  showJuego s
    <> "\n"
            <> (let mensaje = mensajeLetraDescartada (entradaUsuario s) (letrasDescartadas (obtenerIntentos (juego s)))
      in if null mensaje
          then ""
          else mensaje)
    <> "Letras descartadas: " <> letrasDescartadas (obtenerIntentos (juego s))<> "\n"
    <> mensajeOut (estadoIntento s)
    <> "\n"
    <> if juegoFinalizado (juego s) then "Juego finalizado. La palabra es: " <> obtenerPalabraSecreta (juego s) else " " <> "\n"

{-Devuelve las letras descatadas que fuero usadas-}
letrasDescartadas :: [(String, [(Char, Match)])] -> String
letrasDescartadas xs = nub noPerteneceSinExcepciones
  where
    -- Todas las letras marcadas como NoPertenece
    todasNoPertenece = [ c | (_, matches) <- xs, (c, m) <- matches, m == NoPertenece ]

    -- Todas las letras marcadas como Correcto o LugarIncorrecto
    todasExcepciones = [ c | (_, matches) <- xs, (c, m) <- matches, m /= NoPertenece ]

    -- Letras NoPertenece que no están en Excepciones
    noPerteneceSinExcepciones = filter (`notElem` todasExcepciones) todasNoPertenece

{- DAILY MODE GAME -}
-- Función para guardar intentos en un archivo JSON
guardarIntentos :: FilePath -> [(String, [(Char, Match)])] -> IO ()
guardarIntentos ruta intentos = B.writeFile ruta (encode intentos)

-- Función para cargar intentos desde un archivo JSON
cargarIntentos :: FilePath -> IO (Maybe [(String, [(Char, Match)])])
cargarIntentos ruta = decode <$> B.readFile ruta

-- Función para cargar un juego desde un archivo JSON
cargarJuego :: FilePath -> Diccionario -> IO (Maybe Juego)
cargarJuego ruta diccionario = do
  existeArchivo <- doesFileExist ruta
  if not existeArchivo
    then do
      putStrLn $ "El archivo " ++ ruta ++ " no existe. No se puede cargar el juego."
      return Nothing
    else do
      intentos <- cargarIntentos ruta
      case intentos of
        Just intentos -> do
          currentDate <- fmap (show . utctDay) getCurrentTime
          palabra <- obtenerPalabraFecha diccionario currentDate
          let nuevoJuego = iniciarJuego palabra maxIntentos (palabraEnDiccionario diccionario)
          let juegoCargado = actualizarIntentos nuevoJuego intentos
          return (Just juegoCargado)
        Nothing -> return Nothing
  where
    maxIntentos = 5


{- RENDERIZACION -}
{- Devuelve el string del juego -}
showJuego :: State -> String
showJuego s =
  let j = juego s
      largoPalabra = largoPalabraSecreta j
   in if juegoFinalizado j
        then
          concatMap showIntento (obtenerIntentos j)
            <> concatMap showVacio (replicate (intentosRestantes j) largoPalabra)
            <> showSeparador largoPalabra
        else
          concatMap showIntento (obtenerIntentos j)
            <> showSeparador largoPalabra
            <> showEntrada (entradaUsuario s) largoPalabra
            <> concatMap showVacio (replicate (intentosRestantes j - 1) largoPalabra)
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

{- Devuelve el string de un separador de filas (len n) -}
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
mensajeOut (Just CaracterInvalido) = "Error: El cáracter ingresado no es una letra\n"
mensajeOut (Just Valido) = ""

{- Genera mensajes de advertencia para las letras descartadas usadas en la entrada del usuario.-}
mensajeLetraDescartada :: String -> String -> String
mensajeLetraDescartada "" _ = ""
mensajeLetraDescartada (x:rest) descartadas =
    if x `elem` descartadas
        then "¡CUIDADO! La letra '" ++ [x] ++ "' fue descartada.\n" ++ mensajeLetraDescartada rest descartadas
        else mensajeLetraDescartada rest descartadas