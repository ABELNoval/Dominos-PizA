import System.Environment (getArgs)
import Web.Server (runWebServer)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["web"] -> runWebServer
        _ -> do
            putStrLn "Uso: domino-exe [web]"
            putStrLn "  web    - Servidor web en http://localhost:3000"
            putStrLn ""
            putStrLn "Iniciando servidor web..."
            runWebServer
