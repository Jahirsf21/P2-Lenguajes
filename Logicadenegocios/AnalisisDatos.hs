module Logicadenegocios.AnalisisDatos (menuAnalisisDatos, menuBusquedaEspecifica) where
import Logicadenegocios.Estructuras
import Data.List.Split (splitOn)
import Data.Char (isDigit)


-- ===== ANALISIS DE DATOS =====

-- Función para calcular el total de ventas
-- Recibe un objeto Ventas que contenga una lista de ventas y retorna el total de todas las ventas
totalVentas :: Ventas -> Float
totalVentas (Ventas ventas) = totalAuxiliar ventas

-- Función auxiliar que implementa la recursividad para sumar los campos total de una lista de ventas
-- Recibe una lista de ventas y retorna la suma de los totales de cada venta
totalAuxiliar :: [Venta] -> Float
totalAuxiliar [] = 0.0  
totalAuxiliar (ventas:resto) = total ventas + totalAuxiliar resto

-- Función para extraer del año de una cadena de fecha con formato "yyyy-mm-dd"
-- Recibe la fecha como cadena y retorna una cadena de texto con los primeros 4 caracteres que corresponden al año
extraerAnio :: String -> String
extraerAnio fecha = take 4 fecha

-- Función para extraer el mes de una cadena de fecha con formato "yyyy-mm-dd"
-- Recibe la fecha como cadena y retorna una cadena de texto con los primeros 7 caracteres que corresponden al año y mes
extraerMes :: String -> String
extraerMes fecha = take 7 fecha 

-- Ventas agrupadas por año
ventasPorAnio :: Ventas -> [(String, Float)]
ventasPorAnio (Ventas ventas) = agruparVentasPorAnio ventas []

agruparVentasPorAnio :: [Venta] -> [(String, Float)] -> [(String, Float)]
agruparVentasPorAnio [] acum = acum
agruparVentasPorAnio (v:vs) acum =
    let anio = extraerAnio (fecha v)
        nuevoAcum = actualizarAcumulador anio (total v) acum
    in agruparVentasPorAnio vs nuevoAcum

actualizarAcumulador :: String -> Float -> [(String, Float)] -> [(String, Float)]
actualizarAcumulador anio valor [] = [(anio, valor)]
actualizarAcumulador anio valor ((a, v):resto)
    | anio == a = (a, v + valor) : resto
    | otherwise = (a, v) : actualizarAcumulador anio valor resto

-- Ventas agrupadas por mes
ventasPorMes :: Ventas -> [(String, Float)]
ventasPorMes (Ventas ventas) = agruparVentasPorMes ventas []

agruparVentasPorMes :: [Venta] -> [(String, Float)] -> [(String, Float)]
agruparVentasPorMes [] acum = acum
agruparVentasPorMes (v:vs) acum =
    let mes = extraerMes (fecha v)
        nuevoAcumulador = actualizarAcumuladorMes mes (total v) acum
    in agruparVentasPorMes vs nuevoAcumulador

actualizarAcumuladorMes :: String -> Float -> [(String, Float)] -> [(String, Float)]
actualizarAcumuladorMes mes valor [] = [(mes, valor)]
actualizarAcumuladorMes mes valor ((m, v):resto)
    | mes == m  = (m, v + valor) : resto
    | otherwise = (m, v) : actualizarAcumuladorMes mes valor resto

-- Ventas mensuales y anuales combinadas
totalesMensualesAnuales :: Ventas -> ([(String, Float)], [(String, Float)])
totalesMensualesAnuales ventas = (ventasPorMes ventas, ventasPorAnio ventas)

-- Promedio de ventas por categoría por año
promedioVentasPorCategoriaPorAnio :: Ventas -> [(String, String, Float, Int)]
promedioVentasPorCategoriaPorAnio (Ventas ventas) =
    let estadisticas = acumularEstadisticas ventas []
    in calcularPromedios estadisticas

-- Acumular estadísticas por año y categoría
acumularEstadisticas :: [Venta] -> [(String, String, Float, Int)] -> [(String, String, Float, Int)]
acumularEstadisticas [] acum = acum
acumularEstadisticas (v:vs) acum =
    let anio = extraerAnio (fecha v)
        cat = categoria v
        totalVenta = total v
        nuevoAcum = insertarActualizar anio cat totalVenta acum
    in acumularEstadisticas vs nuevoAcum

-- Insertar nueva entrada o actualizar existente
insertarActualizar :: String -> String -> Float -> [(String, String, Float, Int)] -> [(String, String, Float, Int)]
insertarActualizar anio cat valor [] = [(anio, cat, valor, 1)]
insertarActualizar anio cat valor ((a, c, suma, count):resto)
    | anio == a && cat == c = (a, c, suma + valor, count + 1) : resto
    | otherwise = (a, c, suma, count) : insertarActualizar anio cat valor resto

-- Calcular promedios de ventas
calcularPromedios :: [(String, String, Float, Int)] -> [(String, String, Float, Int)]
calcularPromedios [] = []
calcularPromedios ((anio, cat, suma, count):resto) =
    let promedio = if count > 0 then suma / fromIntegral count else 0.0
    in (anio, cat, promedio, count) : calcularPromedios resto


-- Formatear números con 2 decimales
formatearNumero :: Float -> String
formatearNumero num = 
    let entero = floor (num * 100 + 0.5) 
    in show (fromIntegral entero / 100 :: Float)

-- ===== MOSTRAR RESULTADOS =====
--mostrar totales mensuales
mostrarTotalesMensuales :: [(String, Float)] -> IO ()
mostrarTotalesMensuales [] = putStrLn "  No hay datos mensuales."
mostrarTotalesMensuales meses = do
    putStrLn "  Ventas Mensuales:"
    mostrarMesesAuxiliar meses

mostrarMesesAuxiliar :: [(String, Float)] -> IO ()
mostrarMesesAuxiliar [] = return ()
mostrarMesesAuxiliar ((mes, total):resto) = do
    putStrLn $ "Mes: " ++ mes ++ " | Total de ventas: " ++ formatearNumero total
    mostrarMesesAuxiliar resto

--mostrar totales anuales
mostrarTotalesAnuales :: [(String, Float)] -> IO ()
mostrarTotalesAnuales [] = putStrLn "  No hay datos anuales."
mostrarTotalesAnuales anios = do
    putStrLn "  Ventas Anuales:"
    mostrarAniosAuxiliar anios

mostrarAniosAuxiliar :: [(String, Float)] -> IO ()
mostrarAniosAuxiliar [] = return ()
mostrarAniosAuxiliar ((anio, total):resto) = do
    putStrLn $ "Año: " ++ anio ++ " | Total de ventas: " ++ formatearNumero total
    mostrarAniosAuxiliar resto

--mostrar promedios por categoria
mostrarPromediosCategoria :: [(String, String, Float, Int)] -> IO ()
mostrarPromediosCategoria [] = putStrLn "  No hay datos de promedios por categoría."
mostrarPromediosCategoria promedios = do
    imprimir promedios
  where
    imprimir [] = return ()
    imprimir ((anio, cat, prom, cant):ps) = do
      putStrLn $ "Año: " ++ anio ++ " | Categoría: " ++ cat ++ 
                 " | Promedio: " ++ formatearNumero prom ++ 
                 " | Cantidad de ventas: " ++ show cant
      imprimir ps

-- Devuelve el año como Int
obtenerAnioInt :: String -> Int
obtenerAnioInt fecha =
    let partes = splitOn "-" fecha
    in read (head partes) :: Int

-- Devuelve el mes como Int
obtenerMesInt :: String -> Int
obtenerMesInt fecha =
    let partes = splitOn "-" fecha
    in read (partes !! 1) :: Int

obtenerVentasRangoFecha :: (String, String, [Venta]) -> [Venta]
obtenerVentasRangoFecha (_, _, []) = []
obtenerVentasRangoFecha (inicio, fin, x:xs) =
    let
        mesInicio = obtenerMesInt inicio
        anioInicio = obtenerAnioInt inicio
        mesFin = obtenerMesInt fin
        anioFin = obtenerAnioInt fin
        mesActual = obtenerMesInt (fecha x)
        anioActual = obtenerAnioInt (fecha x)
    in
        if (anioActual > anioInicio || (anioActual == anioInicio && mesActual >= mesInicio)) &&
           (anioActual < anioFin || (anioActual == anioFin && mesActual <= mesFin))
           then x : obtenerVentasRangoFecha (inicio, fin, xs)
           else obtenerVentasRangoFecha (inicio, fin, xs)

mostrarVentasRangoFechaAux :: (String, String, [Venta], String) -> IO()
mostrarVentasRangoFechaAux (_, _, [], _) = return ()
mostrarVentasRangoFechaAux (inicio, fin, x:xs, res) =
    let
        contenido = res ++ "Venta " ++ show (venta_id x) ++ "\nFecha: " ++ fecha x ++ "\nProducto: " ++ producto_nombre x ++ "\nTOTAL: " ++ show (total x)
    in do
        putStrLn contenido
        putStrLn "-------------------------"
        mostrarVentasRangoFechaAux (inicio, fin, xs, "")
        

mostrarVentasRangoFecha :: (String, String, [Venta]) -> IO()
mostrarVentasRangoFecha (inicio, fin, ventas) =
    let
        ventasRango = obtenerVentasRangoFecha (inicio, fin, ventas)
    in 
        if null ventasRango then putStrLn "No se encontraron ventas en ese rango"
        else mostrarVentasRangoFechaAux (inicio, fin, ventasRango, "")

comprobarFormato :: String -> Bool
comprobarFormato fecha =
    let 
        mes = obtenerMesInt fecha
    in
        length fecha == 7 && fecha !! 4 == '-' && all isDigit (take 4 fecha) && all isDigit (drop 5 fecha) 
        && mes >= 1 && mes <= 12            

compararFechas :: (String, String) -> Bool
compararFechas (inicio, fin) = 
    let
        mesInicio = obtenerMesInt inicio
        anioInicio = obtenerAnioInt inicio
        mesFin = obtenerMesInt fin
        anioFin = obtenerAnioInt fin
    in
        anioFin > anioInicio || (anioFin == anioInicio && mesFin >= mesInicio)


menuBusquedaEspecifica :: Ventas -> IO ()
menuBusquedaEspecifica ventas@(Ventas listaVentas)
    | null listaVentas = do
        putStrLn "\nNo hay ventas cargadas. Importe datos primero."
        return ()
    | otherwise = do
        putStrLn "Ingrese la fecha de inicio (formato YYYY-MM):"
        inicio <- getLine
        putStrLn "Ingrese la fecha de fin (formato YYYY-MM):"
        fin <- getLine

        if null inicio || null fin
            then do
                putStrLn "\nLas fechas no pueden estar vacías. Intente de nuevo."
                menuBusquedaEspecifica ventas
            else if comprobarFormato inicio && comprobarFormato fin
                then 
                    if compararFechas (inicio, fin) then do
                        putStrLn "\n------- Ventas Encontradas en el Rango de Fechas -------"
                        mostrarVentasRangoFecha (inicio, fin, listaVentas)
                    else do
                        putStrLn "\nLa fecha final no puede ser anterior a la fecha de inicio. Intente de nuevo\n"
                        menuBusquedaEspecifica ventas
                else do
                    putStrLn "\nLas fechas deben de tener el formato YYYY-MM. Intente de nuevo.\n"
                    menuBusquedaEspecifica ventas



-- ===== MENÚ INTERACTIVO =====
menuAnalisisDatos :: Ventas -> IO ()
menuAnalisisDatos ventas@(Ventas listaVentas) = do
  if null listaVentas
    then putStrLn "\n == No hay ventas cargadas. Importe datos primero =="
    else do
      putStrLn "\n========================================="
      putStrLn "      ANÁLISIS DE DATOS"
      putStrLn "========================================="
      putStrLn "1. Total de ventas"
      putStrLn "2. Totales mensuales y anuales"
      putStrLn "3. Promedio de ventas por categoría por año"
      putStrLn "4. Volver al menú principal"
      putStrLn "========================================="
      putStrLn "Seleccione una opción: "
      
      opcion <- getLine
      case opcion of
        "1" -> do
          putStrLn "\n=== Total de ventas ==="
          putStrLn $ "Total: " ++ formatearNumero (totalVentas ventas)
          putStrLn $ "Cantidad de transacciones analizadas: " ++ show (length listaVentas)
          menuAnalisisDatos ventas
        
        "2" -> do
          putStrLn "\n=== Totales de ventas mensuales y anuales ==="
          let (mensuales, anuales) = totalesMensualesAnuales ventas
          mostrarTotalesMensuales mensuales
          putStrLn ""
          mostrarTotalesAnuales anuales
          menuAnalisisDatos ventas
        
        "3" -> do
          putStrLn "\n=== Promedio de ventas por categoría por año ==="
          let promedios = promedioVentasPorCategoriaPorAnio ventas
          mostrarPromediosCategoria promedios
          menuAnalisisDatos ventas
        
        "4" -> putStrLn "Volviendo al menú principal..."
        
        _ -> do
          putStrLn "Opción no válida."
          menuAnalisisDatos ventas
