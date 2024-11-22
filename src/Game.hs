-- src/Game.hs
{-# LANGUAGE OverloadedRecordDot #-}

module Game
  ( Juego,
    EstadoJuego,
    iniciarJuego, -- HECHO
    enviarIntento, -- HECHO
    obtenerIntentos,
    juegoFinalizado, -- HECHO
    ganoJuego, -- HECHO
    largoPalabraSecreta, -- HECHO
    intentosTotales, -- HECHO
  )
where

import Core (Match (..), match)

data EstadoJuego = Gano | Perdio | EnProceso
  deriving (Show, Eq)

data ResultadoIntento = Valido | LargoInvalido | PalabraInvalida | IntentoYaRealizado
  deriving (Show, Eq)

data Juego = Juego
  { target :: String, -- La palabra secreta a adivinar
    maxIntentos :: Int, -- Número máximo de intentos permitidos
    intentos :: [(String, [(Char, Match)])], -- Lista de intentos realizados y sus matches
    estado :: EstadoJuego -- Indica el estado del juego
  }
  deriving (Show)

iniciarJuego :: String -> Int -> Juego
iniciarJuego secret maxInt =
  Juego
    { target = secret,
      maxIntentos = maxInt,
      intentos = [],
      estado = EnProceso
    }

-- | Toma un juego y un intento
--    -> Devuelve Error o Juego (actualizado)
enviarIntento :: Juego -> String -> (ResultadoIntento, Juego)
enviarIntento juego intento
  | not (esLargoValido juego intento) = (LargoInvalido, juego)
  | not (esPalabraValida intento) = (PalabraInvalida, juego)
  | intentoYaRealizado juego intento = (IntentoYaRealizado, juego)
  | otherwise = (Valido, juegoActualizado)
  where
    conIntentos = juego {intentos = juego.intentos ++ [(intento, match juego.target intento)]}
    juegoActualizado = estadoJuego conIntentos

-- Ver si intento esta en corpus?
esLargoValido :: Juego -> String -> Bool
esLargoValido juego intento
  | length intento /= largoPalabraSecreta juego = False
  | otherwise = True

esPalabraValida :: String -> Bool
esPalabraValida intento
    | not (all (`elem` ['a' .. 'z']) intento) = False
    | otherwise = True

intentoYaRealizado :: Juego -> String -> Bool
intentoYaRealizado juego intento = any (\x -> fst x == intento) juego.intentos

largoPalabraSecreta :: Juego -> Int
largoPalabraSecreta juego = length juego.target

estadoJuego :: Juego -> Juego
estadoJuego juego
  | ganoJuego juego = juego {estado = Gano}
  | length juego.intentos < juego.maxIntentos = juego {estado = EnProceso}
  | otherwise = juego {estado = Perdio}

ganoJuego :: Juego -> Bool
ganoJuego juego =
  let ultimoIntento = last juego.intentos  --- (String, [(Char, Match)])
   in all (\(_, m) -> m == Correcto) (snd ultimoIntento)

juegoFinalizado :: Juego -> Bool
juegoFinalizado juego = juego.estado /= EnProceso

intentosTotales :: Juego -> Int
intentosTotales juego = length juego.intentos

obtenerIntentos :: Juego -> [(String, [(Char, Match)])]
obtenerIntentos juego = juego.intentos

