module Logicadenegocios.AnalisisTemporal (menuAnalisisTemporal) where
import Logicadenegocios.Estructuras
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.Time.Calendar (toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.List (sortBy)
import Data.Ord (comparing)


-- ===== ANÁLISIS TEMPORAL =====
-- Función para parsear fecha "YYYY-MM-DD"
parsearFecha :: String -> Maybe Day
parsearFecha fecha = parseTimeM True defaultTimeLocale "%Y-%m-%d" fecha

-- Extraer año-mes (YYYY-MM) de una fecha Day
extraerMes :: Day -> String
extraerMes dia = 
  let (anio, mes, _) = toGregorian dia
  in show anio ++ "-" ++ (if mes < 10 then "0" ++ show mes else show mes)

-- Función para obtener el nombre del día de la semana
obtenerDiaSemana :: Day -> String
obtenerDiaSemana dia = 
  let (_, _, diaNum) = toWeekDate dia
  in case diaNum of
      1 -> "Lunes"
      2 -> "Martes" 
      3 -> "Miércoles"
      4 -> "Jueves"
      5 -> "Viernes"
      6 -> "Sábado"
      7 -> "Domingo"
      _ -> error "Día de la semana fuera de rango (esto no debería ocurrir)"

-- Mes con mayor venta y día de la semana más activo
analisisMensualYSemanal :: Ventas -> (Maybe (String, Float), Maybe (String, Int))
analisisMensualYSemanal (Ventas ventas) =
  let ventasMensuales = calcularVentasMensuales ventas []
      diasSemana = calcularDiasSemana ventas []
      mesMayor = encontrarMaximo ventasMensuales
      diaMasActivo = encontrarMaximoConteo diasSemana
  in (mesMayor, diaMasActivo)

-- Calcular ventas mensuales 
calcularVentasMensuales :: [Venta] -> [(String, Float)] -> [(String, Float)]
calcularVentasMensuales [] acum = acum
calcularVentasMensuales (v:vs) acum =
  case parsearFecha (fecha v) of
    Just dia -> 
      let mes = extraerMes dia
      in calcularVentasMensuales vs (actualizarAcumulador mes (total v) acum)
    Nothing -> calcularVentasMensuales vs acum

-- Calcular días de la semana más activos 
calcularDiasSemana :: [Venta] -> [(String, Int)] -> [(String, Int)]
calcularDiasSemana [] acum = acum
calcularDiasSemana (v:vs) acum =
  case parsearFecha (fecha v) of
    Just dia -> 
      let diaSemana = obtenerDiaSemana dia
      in calcularDiasSemana vs (actualizarConteo diaSemana acum)
    Nothing -> calcularDiasSemana vs acum

-- Actualizar acumulador para ventas
actualizarAcumulador :: String -> Float -> [(String, Float)] -> [(String, Float)]
actualizarAcumulador clave valor [] = [(clave, valor)]
actualizarAcumulador clave valor ((k, v):resto)
  | clave == k = (k, v + valor) : resto
  | otherwise = (k, v) : actualizarAcumulador clave valor resto

-- Actualizar conteo para días de la semana
actualizarConteo :: String -> [(String, Int)] -> [(String, Int)]
actualizarConteo clave [] = [(clave, 1)]
actualizarConteo clave ((k, v):resto)
  | clave == k = (k, v + 1) : resto
  | otherwise = (k, v) : actualizarConteo clave resto

-- Encontrar máximo en lista de ventas
encontrarMaximo :: [(String, Float)] -> Maybe (String, Float)
encontrarMaximo [] = Nothing
encontrarMaximo lista = Just (buscarMaximo (tail lista) (head lista))
  where
    buscarMaximo [] maxActual = maxActual
    buscarMaximo (actual:resto) maxActual
      | snd actual > snd maxActual = buscarMaximo resto actual
      | otherwise = buscarMaximo resto maxActual

-- Encontrar máximo en lista de conteos
encontrarMaximoConteo :: [(String, Int)] -> Maybe (String, Int)
encontrarMaximoConteo [] = Nothing
encontrarMaximoConteo lista = Just (buscarMaximoConteo (tail lista) (head lista))
  where
    buscarMaximoConteo [] maxActual = maxActual
    buscarMaximoConteo (actual:resto) maxActual
      | snd actual > snd maxActual = buscarMaximoConteo resto actual
      | otherwise = buscarMaximoConteo resto maxActual

-- Calcular tasa de crecimiento en trimestre específico
-- Fórmula: ((Ventas Actual - Ventas Anterior) / Ventas Anterior) * 100
calcularTasaCrecimiento :: Ventas -> String -> String -> Maybe Float
calcularTasaCrecimiento (Ventas ventas) trimestreActual trimestreAnterior =
  let ventasActual = calcularVentasTrimestre ventas trimestreActual 0.0
      ventasAnterior = calcularVentasTrimestre ventas trimestreAnterior 0.0
  in if ventasAnterior > 0
     then Just (((ventasActual - ventasAnterior) / ventasAnterior) * 100)
     else if ventasActual == 0 && ventasAnterior == 0
          then Just 0.0  
          else Nothing  

-- Calcular ventas por trimestre 
calcularVentasTrimestre :: [Venta] -> String -> Float -> Float
calcularVentasTrimestre [] _ acum = acum
calcularVentasTrimestre (v:vs) trimestreBuscado acum =
  case parsearFecha (fecha v) of
    Just dia ->
      let trimestreVenta = obtenerTrimestre dia
      in if trimestreVenta == trimestreBuscado
         then calcularVentasTrimestre vs trimestreBuscado (acum + total v)
         else calcularVentasTrimestre vs trimestreBuscado acum
    Nothing -> calcularVentasTrimestre vs trimestreBuscado acum

-- Obtener trimestre de una fecha (formato: YYYY-Q#)
obtenerTrimestre :: Day -> String
obtenerTrimestre dia =
  let (anio, mes, _) = toGregorian dia
      trimestre = case mes of
        m | m >= 1 && m <= 3 -> 1
          | m >= 4 && m <= 6 -> 2
          | m >= 7 && m <= 9 -> 3
          | otherwise -> 4
  in show anio ++ "-Q" ++ show trimestre

-- Resumen de ventas por trimestre
resumenVentasTrimestrales :: Ventas -> [(String, Float)]
resumenVentasTrimestrales (Ventas ventas) =
  let agrupados = agruparVentasTrimestrales ventas []
  in sortBy (comparing fst) agrupados

agruparVentasTrimestrales :: [Venta] -> [(String, Float)] -> [(String, Float)]
agruparVentasTrimestrales [] acum = acum
agruparVentasTrimestrales (v:vs) acum =
  case parsearFecha (fecha v) of
    Just dia ->
      let trimestre = obtenerTrimestre dia
      in agruparVentasTrimestrales vs (actualizarAcumulador trimestre (total v) acum)
    Nothing -> agruparVentasTrimestrales vs acum

-- ===== FUNCIONES DE VISUALIZACIÓN =====
mostrarAnalisisMensualSemanal :: (Maybe (String, Float), Maybe (String, Int)) -> IO ()
mostrarAnalisisMensualSemanal (mesMayor, diaMasActivo) = do

  case mesMayor of
    Just (mes, totalVenta) -> putStrLn $ "  Mes con mayor venta: " ++ mes ++ " - Total: $" ++ formatearNumero totalVenta
    Nothing -> putStrLn "  No hay datos para análisis mensual"
  
  case diaMasActivo of
    Just (dia, conteo) -> putStrLn $ "  Día más activo: " ++ dia ++ " - Transacciones: " ++ show conteo
    Nothing -> putStrLn "  No hay datos para análisis semanal"

mostrarTasaCrecimiento :: Maybe Float -> String -> String -> IO ()
mostrarTasaCrecimiento tasa trimestreActual trimestreAnterior = do
  putStrLn "  --- Tasa de Crecimiento ---"
  putStrLn $ "  Trimestre actual: " ++ trimestreActual
  putStrLn $ "  Trimestre anterior: " ++ trimestreAnterior
  case tasa of
    Just valor -> do
      let tendencia = if valor >= 0 then "crecimiento" else "decrecimiento"
          signo = if valor >= 0 then "+" else ""
      putStrLn $ "  Tasa de " ++ tendencia ++ ": " ++ signo ++ formatearNumero valor ++ "%"
    Nothing -> putStrLn "  No se pudo calcular la tasa de crecimiento (datos insuficientes)"

mostrarResumenTrimestral :: [(String, Float)] -> IO ()
mostrarResumenTrimestral [] = putStrLn "  No hay datos trimestrales."
mostrarResumenTrimestral trimestres = do
  mostrarTrimestres trimestres

mostrarTrimestres :: [(String, Float)] -> IO ()
mostrarTrimestres [] = return ()
mostrarTrimestres ((trimestre, totalVenta):resto) = do
  putStrLn $ "    " ++ trimestre ++ ": $" ++ formatearNumero totalVenta
  mostrarTrimestres resto

-- Formatear números con 2 decimales
formatearNumero :: Float -> String
formatearNumero num = 
  let entero = floor (num * 100 + 0.5)  
  in show (fromIntegral entero / 100 :: Float)

validarFormatoTrimestre :: String -> Bool
validarFormatoTrimestre str = 
  case str of
    [a1, a2, a3, a4, '-', 'Q', t] ->
      todosDigitos [a1, a2, a3, a4] && t `elem` ['1','2','3','4']
    _ -> False
  where
    todosDigitos :: String -> Bool
    todosDigitos [] = True
    todosDigitos (c:cs) = esDigito c && todosDigitos cs
    
    esDigito :: Char -> Bool
    esDigito c = c >= '0' && c <= '9'

-- ===== MENÚ INTERACTIVO DE ANÁLISIS TEMPORAL =====
menuAnalisisTemporal :: Ventas -> IO ()
menuAnalisisTemporal ventas@(Ventas listaVentas) = do
  if null listaVentas
    then putStrLn "\n == No hay ventas cargadas. Importe datos primero =="
    else menuLoopTemporal ventas

menuLoopTemporal :: Ventas -> IO ()
menuLoopTemporal ventas = do
  putStrLn "\n========================================="
  putStrLn "      ANÁLISIS TEMPORAL"
  putStrLn "========================================="
  putStrLn "1. Mes con mayor venta y día más activo"
  putStrLn "2. Calcular tasa de crecimiento trimestral"
  putStrLn "3. Resumen de ventas por trimestre"
  putStrLn "4. Mostrar todos los análisis temporales"
  putStrLn "5. Volver al menú principal"
  putStrLn "========================================="
  putStr "Seleccione una opción: "
  
  opcion <- getLine
  case opcion of
    "1" -> do
      putStrLn "\n=== Mes con mayor venta y día más activo ==="
      let resultado = analisisMensualYSemanal ventas
      mostrarAnalisisMensualSemanal resultado
      menuLoopTemporal ventas  

    "2" -> do
      putStrLn "\n=== Tasa de crecimiento trimestral ==="
      putStrLn "Ingrese el trimestre actual (formato: YYYY-Q#(1-4), ej: 2024-Q3):"
      trimestreActual <- getLine
      putStrLn "Ingrese el trimestre anterior (formato: YYYY-Q#(1-4), ej: 2024-Q2):"
      trimestreAnterior <- getLine
      if not (validarFormatoTrimestre trimestreActual) || not (validarFormatoTrimestre trimestreAnterior)
        then do
          putStrLn "  Formato de trimestre inválido. Use el formato YYYY-Q#."
          menuLoopTemporal ventas
        else do
          let tasa = calcularTasaCrecimiento ventas trimestreActual trimestreAnterior
          mostrarTasaCrecimiento tasa trimestreActual trimestreAnterior
          menuLoopTemporal ventas

    "3" -> do
      putStrLn "\n=== Resumen de ventas por trimestre ==="
      let resumen = resumenVentasTrimestrales ventas
      mostrarResumenTrimestral resumen
      menuLoopTemporal ventas

    "4" -> do
      putStrLn "\n=== Análisis temporal completo ==="
      let analisisMS = analisisMensualYSemanal ventas
      let resumenTrim = resumenVentasTrimestrales ventas
      mostrarAnalisisMensualSemanal analisisMS
      putStrLn ""
      mostrarResumenTrimestral resumenTrim
      menuLoopTemporal ventas

    "5" -> putStrLn "Volviendo al menú principal..."

    _ -> do
      putStrLn "Opción no válida. Intente nuevamente."
      menuLoopTemporal ventas
