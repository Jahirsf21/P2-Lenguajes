module Logicadenegocios.AnalisisDatos (menuAnalisisDatos) where

import Logicadenegocios.Estructuras

-- ===== ANALISIS DE DATOS =====

-- Función para calcular el total de ventas
-- Recibe un objeto Ventas que contenga una lista de ventas y retorna el total de todas las ventas
totalVentas :: Ventas -> Float
totalVentas(Ventas ventas) = totalAuxiliar ventas

-- Función auxiliar que implementa la recursividad para sumar los campos total de una lista de ventas
-- Recibe una lista de ventas y retorna la suma de los totales de cada venta
totalAuxiliar :: [Venta] -> Float
totalAuxiliar [] = 0.0  
totalAuxiliar (ventas:resto) =
  total ventas + totalAuxiliar resto



-- ===== MENÚ INTERACTIVO =====
menuAnalisisDatos :: Ventas -> IO ()
menuAnalisisDatos ventas@(Ventas listaVentas) = do
  if null listaVentas
    then putStrLn "\nNo hay ventas cargadas. Importe datos primero."
    else do
      putStrLn "\n--- Análisis de Datos ---"
      putStrLn "1. Total de Ventas General"
      putStrLn "2. Totales Mensuales y Anuales" 
      putStrLn "3. Promedio de Ventas por Categoría por Año"
      putStrLn "4. Volver al menú principal"
      putStrLn "Seleccione una opción: "
      
      opcion <- getLine
      case opcion of
        "1" -> do
          putStrLn "\n--- Total de ventas ---"
          putStrLn $ "Total: " ++ show (totalVentas ventas)
          putStrLn $ "Cantidad de transacciones analizadas: " ++ show (length listaVentas)
        
        "2" -> do
          putStrLn "\n--- Totales de ventas mensuales y anuales ---"
          let (mensuales, anuales) = totalesMensualesAnuales ventas
          mostrarTotalesMensuales mensuales
          putStrLn ""
          mostrarTotalesAnuales anuales
        
        "3" -> do
          putStrLn "\n--- Promedio de ventas por categoría por año ---"
          let promedios = promedioVentasPorCategoriaPorAnio ventas
          mostrarPromediosCategoria promedios
        
        "4" -> putStrLn "Volviendo al menú principal..."
        
        _ -> do
          putStrLn "Opción no válida."
          menuAnalisisDatos ventas