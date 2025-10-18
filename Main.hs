module Main where
import Logicadenegocios.ImportacionDatos (menuImportarDatos)
import Logicadenegocios.ProcesamientoDatos (menuProcesadoDatos)
import Logicadenegocios.AnalisisTemporal (menuAnalisisTemporal)
import Logicadenegocios.Estructuras
import Logicadenegocios.Estadisticas (menuEstadisticas, ventaMasAlta)
import Logicadenegocios.AnalisisDatos (menuAnalisisDatos, menuBusquedaEspecifica)


main :: IO ()
main = menuPrincipal (Ventas [])

menuPrincipal :: Ventas -> IO ()
menuPrincipal ventas = do
    putStrLn "\n----- Análisis de Ventas -----"
    putStrLn "1. Importar Datos"
    putStrLn "2. Procesamiento de Datos"
    putStrLn "3. Análisis de Datos"
    putStrLn "4. Análisis Temporal"
    putStrLn "5. Búsqueda Específica"
    putStrLn "6. Estadisticas"
    putStrLn "7. Salir"
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
            menuAnalisisTemporal ventas
            menuPrincipal ventas
        "5" -> do
            menuBusquedaEspecifica ventas 
            menuPrincipal ventas
        "6" -> do
            menuEstadisticas ventas
            menuPrincipal ventas
        "7" -> putStrLn "Saliendo..."
        _   -> do
            putStrLn "Opción no válida, intente de nuevo."
            menuPrincipal ventas