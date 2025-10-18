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

-- Función principal para agrupar las ventas y sumar sus totales por año.
-- Recibe un valor de tipo Ventas y retorna una lista de tuplas con el año y el total de ventas para ese año.
ventasPorAnio :: Ventas -> [(String, Float)]
ventasPorAnio (Ventas ventas) = agruparVentasPorAnio ventas []

-- Función recursiva  para agrupar y sumar los totales de las ventas por año.
-- Recorre recursivamente una lista de ventas (Venta), extrayendo el año de cada venta y actualizando un acumulador
-- (una lista de tuplas) con la suma de los totales de las ventas para ese año.
-- Recibe una lista de objetos de venta `[Venta]` y una lista de tuplas `[(String, Float)]` final con los totales de ventas
agruparVentasPorAnio :: [Venta] -> [(String, Float)] -> [(String, Float)]
agruparVentasPorAnio [] acum = acum
agruparVentasPorAnio (v:vs) acum =
    let anio = extraerAnio (fecha v)
        nuevoAcum = actualizarAcumulador anio (total v) acum
    in agruparVentasPorAnio vs nuevoAcum

-- Función auxiliar que busca y actualiza el total de un año específico en el acumulador.
-- Recibe el año, el valor a sumar y el acumulador actual y una nueva lista de tuplas `[(String, Float)]` con el total del 
-- año actualizado.
actualizarAcumulador :: String -> Float -> [(String, Float)] -> [(String, Float)]
actualizarAcumulador anio valor [] = [(anio, valor)]
actualizarAcumulador anio valor ((a, v):resto)
    | anio == a = (a, v + valor) : resto
    | otherwise = (a, v) : actualizarAcumulador anio valor resto

-- Función principal para agrupar las ventas y sumar sus totales por mes.
-- Recibe un valor de tipo `Ventas`, que contiene la lista de ventas a procesar y retorna una lista de tuplas con el mes y el 
-- total de ventas para ese mes.
ventasPorMes :: Ventas -> [(String, Float)]
ventasPorMes (Ventas ventas) = agruparVentasPorMes ventas []

-- Función  para procesar una lista de ventas y agrupar sus totales por mes.
-- Recibe una lista de objetos de venta `[Venta]` y retorna una lista de tuplas `[(String, Float)]` final con los totales de ventas
--agrupados por mes.
agruparVentasPorMes :: [Venta] -> [(String, Float)] -> [(String, Float)]
agruparVentasPorMes [] acum = acum
agruparVentasPorMes (v:vs) acum =
    let mes = extraerMes (fecha v)
        nuevoAcumulador = actualizarAcumuladorMes mes (total v) acum
    in agruparVentasPorMes vs nuevoAcumulador

-- Función auxiliar que busca y actualiza el total de un mes específico en el acumulador.
-- Recibe el `String` del mes a buscar o agregar, el `Float` del valor total de la venta a sumar y el acumulador actual `[(String, Float)]`. 
-- Retorna una nueva lista de tuplas `[(String, Float)]` con el total del mes actualizado.
actualizarAcumuladorMes :: String -> Float -> [(String, Float)] -> [(String, Float)]
actualizarAcumuladorMes mes valor [] = [(mes, valor)]
actualizarAcumuladorMes mes valor ((m, v):resto)
    | mes == m  = (m, v + valor) : resto
    | otherwise = (m, v) : actualizarAcumuladorMes mes valor resto

-- Función para calcular los totales de ventas agrupados por mes y por año simultáneamente.
-- Recibe un valor de tipo `Ventas`, que contiene la lista de ventas a procesar y retorna una tupla `([(String, Float)], [(String, Float)])`.
totalesMensualesAnuales :: Ventas -> ([(String, Float)], [(String, Float)])
totalesMensualesAnuales ventas = (ventasPorMes ventas, ventasPorAnio ventas)

-- Función principal para calcular el promedio de ventas por cada categoría y año.
-- Recibe un valor de tipo `Ventas` y retorna una lista de tuplas con el año, la categoría, el promedio de ventas y el conteo de ventas.
promedioVentasPorCategoriaPorAnio :: Ventas -> [(String, String, Float, Int)]
promedioVentasPorCategoriaPorAnio (Ventas ventas) =
    let estadisticas = acumularEstadisticas ventas []
    in calcularPromedios estadisticas

-- Función para iterar sobre la lista de ventas y acumular la suma de totales y el conteo.
-- Recibe una lista de objetos de venta `[Venta]` y un acumulador `[(String, String, Float, Int)]` que almacena 
-- (Año, Categoría, Suma_Total, Conteo). Retorna el acumulador final `[(String, String, Float, Int)]` con la suma total y el conteo 
-- de ventas para cada combinación única de Año y Categoría.
acumularEstadisticas :: [Venta] -> [(String, String, Float, Int)] -> [(String, String, Float, Int)]
acumularEstadisticas [] acum = acum
acumularEstadisticas (v:vs) acum =
    let anio = extraerAnio (fecha v)
        cat = categoria v
        totalVenta = total v
        nuevoAcum = insertarActualizar anio cat totalVenta acum
    in acumularEstadisticas vs nuevoAcum

-- Función auxiliar para gestionar la inserción o actualización de estadísticas de ventas.
-- Recibe el año, la categoría, el valor de la venta y el acumulador actual y retorna una nueva lista de tuplas
-- con la entrada actualizada o insertada.
insertarActualizar :: String -> String -> Float -> [(String, String, Float, Int)] -> [(String, String, Float, Int)]
insertarActualizar anio cat valor [] = [(anio, cat, valor, 1)]
insertarActualizar anio cat valor ((a, c, suma, count):resto)
    | anio == a && cat == c = (a, c, suma + valor, count + 1) : resto
    | otherwise = (a, c, suma, count) : insertarActualizar anio cat valor resto

-- Función para transformar la lista de estadísticas acumuladas en promedios de ventas.
-- Recibe una lista de tuplas `[(String, String, Float, Int)]` donde el tercer elemento es la suma total y el cuarto 
-- es el conteo. Retorna una lista de tuplas `[(String, String, Float, Int)]` donde el tercer elemento es el promedio de ventas.
calcularPromedios :: [(String, String, Float, Int)] -> [(String, String, Float, Int)]
calcularPromedios [] = []
calcularPromedios ((anio, cat, suma, count):resto) =
    let promedio = if count > 0 then suma / fromIntegral count else 0.0
    in (anio, cat, promedio, count) : calcularPromedios resto

-- Función para formatear un número de punto flotante a una representación de cadena con 2 decimales.
-- Recibe un número de punto flotante `Float` y retorna su representación en cadena `String` con dos decimales.
formatearNumero :: Float -> String
formatearNumero num = 
    let entero = floor (num * 100 + 0.5) 
    in show (fromIntegral entero / 100 :: Float)

-- ===== MOSTRAR RESULTADOS =====
-- Función para mostrar los totales de ventas agrupados por mes.
-- Recibe una lista de tuplas con el mes y el total de ventas y muestra la información en la consola.
mostrarTotalesMensuales :: [(String, Float)] -> IO ()
mostrarTotalesMensuales [] = putStrLn "  No hay datos mensuales."
mostrarTotalesMensuales meses = do
    putStrLn "  Ventas Mensuales:"
    mostrarMesesAuxiliar meses

-- Función auxiliar recursiva para imprimir los datos de ventas por mes.
-- Recibe una lista de tuplas `[(String, Float)]` de totales mensuales y muestra cada mes y su total de ventas en consola.
mostrarMesesAuxiliar :: [(String, Float)] -> IO ()
mostrarMesesAuxiliar [] = return ()
mostrarMesesAuxiliar ((mes, total):resto) = do
    putStrLn $ "Mes: " ++ mes ++ " | Total de ventas: " ++ formatearNumero total
    mostrarMesesAuxiliar resto

-- Función para mostrar los totales de ventas agrupados por año.
-- Recibe una lista de tuplas `[(String, Float)]`, donde el String es el año y el Float es el total de ventas.
-- Retorna una acción de IO que imprime la información en la consola.
mostrarTotalesAnuales :: [(String, Float)] -> IO ()
mostrarTotalesAnuales [] = putStrLn "  No hay datos anuales."
mostrarTotalesAnuales anios = do
    putStrLn "  Ventas Anuales:"
    mostrarAniosAuxiliar anios

-- Función auxiliar recursiva para imprimir los datos de ventas por año.
-- Recibe una lista de tuplas `[(String, Float)]` de totales anuales.
-- Retorna una acción de IO que imprime cada año y su total de ventas.
mostrarAniosAuxiliar :: [(String, Float)] -> IO ()
mostrarAniosAuxiliar [] = return ()
mostrarAniosAuxiliar ((anio, total):resto) = do
    putStrLn $ "Año: " ++ anio ++ " | Total de ventas: " ++ formatearNumero total
    mostrarAniosAuxiliar resto

-- Función para mostrar los promedios de ventas agrupados por año y categoría.
-- Recibe una lista de tuplas `[(String, String, Float, Int)]` que contienen (Año, Categoría, Promedio, Conteo).
-- Retorna una acción de IO que imprime el detalle de los promedios en la consola.
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
