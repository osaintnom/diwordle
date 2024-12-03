module Parser (parseConfig) where

import CLI (Config(..), Modo(..))

import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Time(Day)
import System.Directory.Internal.Prelude (exitFailure)

parseConfig :: [String] -> IO Config
parseConfig args = parseConfigAux defaultConfig args
    where
        parseConfigAux :: Config -> [String] -> IO Config
        parseConfigAux config [] = return config
        parseConfigAux config (arg:args') 
            | arg == "--daily" = dailyConfig (safeHead args') config >>= \newConfig -> parseConfigAux newConfig (safeTail args')
            | arg == "--intentos" = intentosConfig (safeHead args') config >>= \newConfig -> parseConfigAux newConfig (safeTail args')
            | arg == "--random" = parseConfigAux (config { modo = Random }) args'
            | arg == "--filename" = filenameConfig (safeHead args') config >>= \newConfig -> parseConfigAux newConfig (safeTail args')
            | arg == "--palabra" = palabraConfig (safeHead args') config >>= \newConfig -> parseConfigAux newConfig (safeTail args')
            | otherwise = printError "Argumento invalido"

defaultConfig :: Config
defaultConfig = Config { modo = Daily Nothing, filename = "diccionario.txt", intentos = 5 }

dailyConfig :: Maybe String -> Config -> IO Config
dailyConfig Nothing config = return config { modo = Daily Nothing }
dailyConfig (Just fecha) config
  | fechaValida fecha = return config { modo = Daily (Just fecha) }
  | otherwise = printError "Fecha invalida"

intentosConfig :: Maybe String -> Config -> IO Config
intentosConfig Nothing config = return config
intentosConfig (Just n) config
  | intentosValido n = return config { intentos = read n }
  | otherwise = printError "Numero de intentos invalido"

filenameConfig :: Maybe String -> Config -> IO Config
filenameConfig Nothing _ = printError "Falta el nombre del archivo"
filenameConfig (Just flname) config = return config { filename = flname } 

palabraConfig :: Maybe String -> Config -> IO Config
palabraConfig Nothing _ = printError "Falta la palabra"
palabraConfig (Just palabra) config = return config { modo = FixedWord palabra }

printError :: String -> IO Config
printError msg = do
    putStrLn msg
    putStrLn "Uso: ./diwordle [--daily [YYYY-MM-DD]] [--intentos N] [--random] [--filename FILENAME] [--palabra PALABRA]"
    exitFailure

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

intentosValido :: String -> Bool
intentosValido n = 
    case reads n :: [(Int, String)] of
        [(n', "")] -> n' > 0
        _         -> False

fechaValida :: String -> Bool
fechaValida fecha =
  case iso8601ParseM fecha :: Maybe Day of
    Just _  -> True
    Nothing -> False
