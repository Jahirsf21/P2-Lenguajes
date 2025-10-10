module Main where

import Logicadenegocios.ImportacionDatos (menuImportarDatos)

main :: IO ()
main = do
    putStrLn "----- Análisis de Ventas -----"
    putStrLn "1. Importar Datos"
    putStrLn "2. Procesamiento de Datos"
    putStrLn "3. Análisis de Datos"
    putStrLn "4. Análisis Temporal"
    putStrLn "5. Salir"
    putStrLn "Seleccione una opción: "
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Importando datos..."
            menuImportarDatos
            main
        "2" -> do
            putStrLn "Procesando datos..."
            main
        "3" -> do
            putStrLn "Analizando datos..."
            main
        "4" -> do
            putStrLn "Análisis temporal..."
            main
        "5" -> putStrLn "Saliendo..."
        _   -> do
            putStrLn "Opción no válida, intente de nuevo."
            main
