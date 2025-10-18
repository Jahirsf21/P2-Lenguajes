module Logicadenegocios.AnalisisTemporal (menuAnalisisTemporal) where
import Logicadenegocios.Estructuras
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.Time.Calendar (toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.List (sortBy)
import Data.Ord (comparing)


-- ===== ANÁLISIS TEMPORAL =====
-- Función para convertir una cadena de texto en formato "YYYY-MM-DD" a un tipo 'Day'.
-- Recibe una cadena de texto `String` con el formato de fecha "YYYY-MM-DD" y retorna un valor de tipo `Maybe Day`.
parsearFecha :: String -> Maybe Day
parsearFecha fecha = parseTimeM True defaultTimeLocale "%Y-%m-%d" fecha

-- Función para extraer el año y el mes de un tipo 'Day' y formatearlos como "YYYY-MM".
-- Recibe un valor de tipo `Day` (la fecha) y retorna una cadena de texto `String` con el formato "YYYY-MM".
extraerMes :: Day -> String
extraerMes dia = 
  let (anio, mes, _) = toGregorian dia
  in show anio ++ "-" ++ (if mes < 10 then "0" ++ show mes else show mes)

-- Función para determinar y retornar el nombre del día de la semana para una fecha dada.
-- Recibe un valor de tipo `Day` (la fecha) y retorna una cadena de texto `String` con el nombre del día de la semana.
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

-- Función principal para identificar el mes con mayor volumen de ventas y el día de la semana más activo.
-- Recibe un valor de tipo `Ventas`, que contiene la lista de ventas a analizar y retorna una tupla `(Maybe (String, Float), Maybe (String, Int))`, 
-- donde el primer `Maybe` contiene la tupla `(Mes, TotalVentas)` del mes con la mayor venta, o `Nothing` y el segundo `Maybe` contiene la tupla 
-- `(DiaSemana, ConteoVentas)` del día más activo, o `Nothing`.
analisisMensualYSemanal :: Ventas -> (Maybe (String, Float), Maybe (String, Int))
analisisMensualYSemanal (Ventas ventas) =
  let ventasMensuales = calcularVentasMensuales ventas []
      diasSemana = calcularDiasSemana ventas []
      mesMayor = encontrarMaximo ventasMensuales
      diaMasActivo = encontrarMaximoConteo diasSemana
  in (mesMayor, diaMasActivo)

-- Función recursiva para agrupar y sumar los totales de ventas por mes (Año-Mes).
-- Recibe una lista de objetos de venta `[Venta]`. Un acumulador `[(String, Float)]` parcial, donde el String es el mes (YYYY-MM)
-- y el Float es el total de ventas para ese mes. Retorna una lista de tuplas `[(String, Float)]` con el total de ventas agrupado por mes.
calcularVentasMensuales :: [Venta] -> [(String, Float)] -> [(String, Float)]
calcularVentasMensuales [] acum = acum
calcularVentasMensuales (v:vs) acum =
  case parsearFecha (fecha v) of
    Just dia -> 
      let mes = extraerMes dia
      in calcularVentasMensuales vs (actualizarAcumulador mes (total v) acum)
    Nothing -> calcularVentasMensuales vs acum

-- Función recursiva para contar la cantidad de ventas agrupadas por día de la semana.
-- Recibe una lista de objetos de venta `[Venta]`. Un acumulador `[(String, Int)]` parcial, donde el String es el nombre del día de la semana
-- y el Int es la cantidad de ventas. Retorna una lista de tuplas `[(String, Int)]` con el conteo de ventas agrupado por día de la semana. 
calcularDiasSemana :: [Venta] -> [(String, Int)] -> [(String, Int)]
calcularDiasSemana [] acum = acum
calcularDiasSemana (v:vs) acum =
  case parsearFecha (fecha v) of
    Just dia -> 
      let diaSemana = obtenerDiaSemana dia
      in calcularDiasSemana vs (actualizarConteo diaSemana acum)
    Nothing -> calcularDiasSemana vs acum

-- Función recursiva para sumar un valor a una clave existente en una lista de tuplas (acumulador).
-- Recibe: la `String` de la clave a buscar o agregar, el `Float` del valor a sumar y el acumulador actual `[(String, Float)]`.
-- Retorna una nueva lista de tuplas `[(String, Float)]` con el valor actualizado o insertado.
actualizarAcumulador :: String -> Float -> [(String, Float)] -> [(String, Float)]
actualizarAcumulador clave valor [] = [(clave, valor)]
actualizarAcumulador clave valor ((k, v):resto)
  | clave == k = (k, v + valor) : resto
  | otherwise = (k, v) : actualizarAcumulador clave valor resto

-- Función recursiva para incrementar el conteo de una clave existente en una lista de tuplas (acumulador).
-- Recibe la `String` de la clave a buscar o agregar y el acumulador actual `[(String, Int)]` (Clave, Conteo).
-- Retorna una nueva lista de tuplas `[(String, Int)]` con el conteo actualizado o insertado.
actualizarConteo :: String -> [(String, Int)] -> [(String, Int)]
actualizarConteo clave [] = [(clave, 1)]
actualizarConteo clave ((k, v):resto)
  | clave == k = (k, v + 1) : resto
  | otherwise = (k, v) : actualizarConteo clave resto

-- Función para encontrar el elemento con el valor Float máximo en una lista de tuplas (Clave, Valor).
-- Recibe una lista de tuplas `[(String, Float)]` (Clave, Valor) y retorna un `Maybe (String, Float)` la tupla con el valor máximo,
-- o `Nothing` si la lista de entrada está vacía.
encontrarMaximo :: [(String, Float)] -> Maybe (String, Float)
encontrarMaximo [] = Nothing
encontrarMaximo lista = Just (buscarMaximo (tail lista) (head lista))
  where
    buscarMaximo [] maxActual = maxActual
    buscarMaximo (actual:resto) maxActual
      | snd actual > snd maxActual = buscarMaximo resto actual
      | otherwise = buscarMaximo resto maxActual

-- Función para encontrar el elemento con el valor Int (conteo) máximo en una lista de tuplas (Clave, Conteo).
-- Recibe una lista de tuplas `[(String, Int)]` (Clave, Conteo). Retorna un `Maybe (String, Int)`  la tupla con el conteo máximo,
-- o `Nothing` si la lista de entrada está vacía.
encontrarMaximoConteo :: [(String, Int)] -> Maybe (String, Int)
encontrarMaximoConteo [] = Nothing
encontrarMaximoConteo lista = Just (buscarMaximoConteo (tail lista) (head lista))
  where
    buscarMaximoConteo [] maxActual = maxActual
    buscarMaximoConteo (actual:resto) maxActual
      | snd actual > snd maxActual = buscarMaximoConteo resto actual
      | otherwise = buscarMaximoConteo resto maxActual


-- Fórmula: ((Ventas Actual - Ventas Anterior) / Ventas Anterior) * 100
-- Función para calcular la tasa de crecimiento de ventas entre dos trimestres específicos.
-- Recibe un valor de tipo `Ventas`, que contiene la lista de ventas, el `String` del trimestre actual (e.g., "2024-Q2") y el `String` del trimestre anterior 
-- Retorna un `Maybe Float`. Retorna `Just` la tasa de crecimiento porcentual, o `Nothing` si
-- la división por cero resulta en un crecimiento indefinido o no se puede calcular.
calcularTasaCrecimiento :: Ventas -> String -> String -> Maybe Float
calcularTasaCrecimiento (Ventas ventas) trimestreActual trimestreAnterior =
  let ventasActual = calcularVentasTrimestre ventas trimestreActual 0.0
      ventasAnterior = calcularVentasTrimestre ventas trimestreAnterior 0.0
  in if ventasAnterior > 0
     then Just (((ventasActual - ventasAnterior) / ventasAnterior) * 100)
     else if ventasActual == 0 && ventasAnterior == 0
          then Just 0.0  
          else Nothing  

-- Función recursiva para sumar el total de ventas de un trimestre específico.
-- Recibe una lista de objetos de venta `[Venta]`, el `String` del trimestre a buscar y un acumulador `Float` para la suma parcial.
-- Retorna un `Float` que representa el total de ventas para el trimestre especificado.
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

-- Función para determinar el trimestre del año de una fecha y formatearlo como YYYY-Q#.
-- Recibe un valor de tipo `Day` (la fecha) y retorna una cadena de texto `String` con el identificador del trimestre.
obtenerTrimestre :: Day -> String
obtenerTrimestre dia =
  let (anio, mes, _) = toGregorian dia
      trimestre = case mes of
        m | m >= 1 && m <= 3 -> 1
          | m >= 4 && m <= 6 -> 2
          | m >= 7 && m <= 9 -> 3
          | otherwise -> 4
  in show anio ++ "-Q" ++ show trimestre

-- Función principal para obtener el resumen de ventas agrupadas por trimestre y ordenarlas.
-- Recibe un valor de tipo `Ventas`, que encapsula la lista de ventas a procesar y retorna una lista de tuplas `[(String, Float)]`, 
-- donde el String es el identificador del trimestre y el Float es el total de ventas, ordenadas por trimestre.
resumenVentasTrimestrales :: Ventas -> [(String, Float)]
resumenVentasTrimestrales (Ventas ventas) =
  let agrupados = agruparVentasTrimestrales ventas []
  in sortBy (comparing fst) agrupados

-- Función recursiva para iterar sobre la lista de ventas y acumular los totales por trimestre.
-- Recibe una lista de objetos de venta `[Venta]` y un acumulador `[(String, Float)]` parcial (Trimestre, TotalVentas).
-- Retorna una lista de tuplas `[(String, Float)]` con la suma total de ventas por cada trimestre.
agruparVentasTrimestrales :: [Venta] -> [(String, Float)] -> [(String, Float)]
agruparVentasTrimestrales [] acum = acum
agruparVentasTrimestrales (v:vs) acum =
  case parsearFecha (fecha v) of
    Just dia ->
      let trimestre = obtenerTrimestre dia
      in agruparVentasTrimestrales vs (actualizarAcumulador trimestre (total v) acum)
    Nothing -> agruparVentasTrimestrales vs acum

-- ===== FUNCIONES DE VISUALIZACIÓN =====
-- Función para imprimir en consola los resultados del mes con mayor venta y el día de la semana más activo.
-- Recibe una tupla `(Maybe (String, Float), Maybe (String, Int))`. El primer `Maybe` contiene el mes de mayor venta y su total y 
-- el segundo `Maybe` contiene el día más activo y su conteo de ventas.
-- Retorna una acción de IO que imprime el análisis en la consola.
mostrarAnalisisMensualSemanal :: (Maybe (String, Float), Maybe (String, Int)) -> IO ()
mostrarAnalisisMensualSemanal (mesMayor, diaMasActivo) = do
  case mesMayor of
    Just (mes, totalVenta) -> putStrLn $ "  Mes con mayor venta: " ++ mes ++ " - Total: $" ++ formatearNumero totalVenta
    Nothing -> putStrLn "  No hay datos para análisis mensual"
  
  case diaMasActivo of
    Just (dia, conteo) -> putStrLn $ "  Día más activo: " ++ dia ++ " - Transacciones: " ++ show conteo
    Nothing -> putStrLn "  No hay datos para análisis semanal"

-- Función para mostrar el resultado del cálculo de la tasa de crecimiento trimestral.
-- Recibe un `Maybe Float` con la tasa de crecimiento, un `String` del trimestre actual y un `String` del trimestre anterior.
-- Retorna una acción de IO que imprime el resultado en la consola.
mostrarTasaCrecimiento :: Maybe Float -> String -> String -> IO ()
mostrarTasaCrecimiento tasa trimestreActual trimestreAnterior = do
  putStrLn "\n  === Tasa de Crecimiento ==="
  putStrLn $ "  Trimestre actual: " ++ trimestreActual
  putStrLn $ "  Trimestre anterior: " ++ trimestreAnterior
  case tasa of
    Just valor -> do
      let tendencia = if valor >= 0 then "crecimiento" else "decrecimiento"
          signo = if valor >= 0 then "+" else ""
      putStrLn $ "  Tasa de " ++ tendencia ++ ": " ++ signo ++ formatearNumero valor ++ "%"
    Nothing -> putStrLn "  No se pudo calcular la tasa de crecimiento (datos insuficientes)"

-- Función principal para mostrar el resumen de ventas agrupadas por trimestre.
-- Recibe una lista de tuplas `[(String, Float)]` de totales trimestrales (Trimestre, TotalVenta) y retorna una acción de IO
-- que imprime el resumen trimestral.
mostrarResumenTrimestral :: [(String, Float)] -> IO ()
mostrarResumenTrimestral [] = putStrLn "  No hay datos trimestrales."
mostrarResumenTrimestral trimestres = do
  mostrarTrimestres trimestres

-- Función auxiliar recursiva para imprimir los datos de ventas por trimestre.
-- Recibe una lista de tuplas `[(String, Float)]` de totales trimestrales y retorna una acción de IO que imprime cada trimestre y 
-- su total de ventas.
mostrarTrimestres :: [(String, Float)] -> IO ()
mostrarTrimestres [] = return ()
mostrarTrimestres ((trimestre, totalVenta):resto) = do
  putStrLn $ "    " ++ trimestre ++ ": $" ++ formatearNumero totalVenta
  mostrarTrimestres resto

-- Función para formatear un número de punto flotante a una representación de cadena con 2 decimales.
-- Recibe un número de punto flotante `Float` y retorna su representación en cadena `String` con dos decimales.
formatearNumero :: Float -> String
formatearNumero num = 
  let entero = floor (num * 100 + 0.5)  
  in show (fromIntegral entero / 100 :: Float)

-- Función para validar si una cadena de texto sigue el formato de trimestre "YYYY-Q#" y si el año es realista.
-- Recibe una cadena de texto `String`, la posible clave de trimestre.
-- Retorna un valor booleano `Bool`. Retorna True si la cadena cumple con el formato y las restricciones del año, y False en caso contrario.
validarFormatoTrimestre :: String -> Bool
validarFormatoTrimestre str = 
  case str of
    [a1, a2, a3, a4, '-', 'Q', t] ->
      let anio = [a1, a2, a3, a4]
      in todosDigitos anio && 
        esAnioValido (leerNumero anio) && 
        t `elem` ['1','2','3','4']
    _ -> False
  where
    todosDigitos :: String -> Bool
    -- Función para verifica recursivamente si todos los caracteres de una cadena son dígitos.
    todosDigitos [] = True
    todosDigitos (c:cs) = esDigito c && todosDigitos cs

    esDigito :: Char -> Bool
    -- Función para verifica si un carácter está en el rango de '0' a '9'.
    esDigito c = c >= '0' && c <= '9'

    esAnioValido :: Maybe Int -> Bool
    -- Función para verifica si un año (Int, dentro de un Maybe) está en el rango [1980, 2025].
    esAnioValido (Just anio) = anio >= 1980 && anio <= 2025
    esAnioValido Nothing = False

    leerNumero :: String -> Maybe Int
    -- Función para iniciar el proceso de conversión de String a Int.
    leerNumero str = leerNumeroRecursivo str 0

    leerNumeroRecursivo :: String -> Int -> Maybe Int
    -- Función para conviertir recursivamente una cadena de dígitos en un entero.
    leerNumeroRecursivo [] acum = Just acum
    leerNumeroRecursivo (c:cs) acum
      | esDigito c = leerNumeroRecursivo cs (acum * 10 + charAInt c)
      | otherwise = Nothing

    charAInt :: Char -> Int
    -- Función para convertir un carácter dígito a su valor entero.
    charAInt c = fromEnum c - fromEnum '0'

-- ===== MENÚ INTERACTIVO DE ANÁLISIS TEMPORAL =====
menuAnalisisTemporal :: Ventas -> IO ()
menuAnalisisTemporal ventas@(Ventas listaVentas) = do
  if null listaVentas
    then putStrLn "\n == No hay ventas cargadas. Importe datos primero =="
    else menuLoopTemporal ventas

menuLoopTemporal :: Ventas -> IO ()
menuLoopTemporal ventas = do
  putStrLn "\n========================================="
  putStrLn "            ANÁLISIS TEMPORAL"
  putStrLn "========================================="
  putStrLn "1. Mes con mayor venta y día más activo"
  putStrLn "2. Calcular tasa de crecimiento trimestral"
  putStrLn "3. Resumen de ventas por trimestre"
  putStrLn "4. Volver al menú principal"
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

    "4" -> do putStrLn "Volviendo al menú principal..."

    _ -> do
      putStrLn "Opción no válida. Intente nuevamente."
      menuLoopTemporal ventas
