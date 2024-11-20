-- src/Core.hs
module Core (
    runCore
) where

import Game (getGameMessage)

runCore :: IO ()
runCore = putStrLn (getGameMessage)
