module Main (main) where

import Lib
import System.IO

main :: IO ()
main = do
    hSetEncoding stdout utf8
    let fichas = generarFichas
    fichasBarajadas <- barajarFichas fichas
    
    let nJugadores = 4 -- Por ahora, jugamos con 4 jugadores
    let (jugadores, pozo) = repartirManos fichasBarajadas nJugadores

    putStrLn "--- Manos de los jugadores ---"
    mapM_ print jugadores
    
    putStrLn "\n--- Fichas restantes en el pozo ---"
    print pozo
