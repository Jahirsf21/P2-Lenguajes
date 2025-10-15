module Main where
import Logicadenegocios.ImportacionDatos (menuImportarDatos)
import Logicadenegocios.ProcesamientoDatos (menuProcesadoDatos)
import Logicadenegocios.AnalisisTemporal (menuAnalisisDatosTemporal)
import Logicadenegocios.Estructuras
import Logicadenegocios.Estadisticas (menuEstadisticas)


main :: IO ()
main = menuPrincipal (Ventas [])

menuPrincipal :: Ventas -> IO ()
menuPrincipal ventas = do
    putStrLn "\n----- Análisis de Ventas -----"
    putStrLn "1. Importar Datos"
    putStrLn "2. Procesamiento de Datos"
    putStrLn "3. Análisis de Datos"
    putStrLn "4. Análisis Temporal"
    putStrLn "5. Estadisticas"
    putStrLn "6. Salir"
    putStrLn "Seleccione una opción: "
    opcion <- getLine
    case opcion of
        "1" -> do
            nuevasVentas <- menuImportarDatos ventas  
            menuPrincipal nuevasVentas
        "2" -> do
            ventasProcesadas <- menuProcesadoDatos ventas
            menuPrincipal ventasProcesadas
        "3" -> do
            menuAnalisisDatos ventas
            menuPrincipal ventas
        "4" -> do
            ventasAnalizadas <- menuAnalisisDatosTemporal ventas
            menuPrincipal ventasAnalizadas
        "5" -> do
            menuEstadisticas ventas
            menuPrincipal ventas
        "6" -> putStrLn "Saliendo..."
        _   -> do
            putStrLn "Opción no válida, intente de nuevo."
            menuPrincipal ventas