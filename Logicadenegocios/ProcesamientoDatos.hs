module Logicadenegocios.ProcesamientoDatos (menuProcesadoDatos, mostrarVenta) where
import Logicadenegocios.Estructuras
import Data.List (nubBy)

-- Función para ordenar una lista usando quicksort
-- Retorna una lista ordenada de menor a mayor
quicksort :: [Float] -> [Float]
quicksort [] = []
quicksort (x:xs) = 
  let menores = quicksort [y | y <- xs, y <= x]
      mayores = quicksort [y | y <- xs, y > x]
  in menores ++ [x] ++ mayores

-- Función auxiliar para contar cuántas veces aparece un elemento en una lista
contador :: Float -> [Float] -> Int
contador _ [] = 0
contador x (y:ys)
  | x == y    = 1 + contador x ys
  | otherwise = contador x ys

-- Función para contar la frecuencia de cada elemento en una lista
frecuencias :: [Float] -> [(Float, Int)]
frecuencias [] = []
frecuencias (x:xs)
  | existe x xs = frecuencias xs  
  | otherwise   = (x, contador x (x:xs)) : frecuencias xs

--- Función auxiliar para verificar si un elemento existe en una lista
existe :: Float -> [Float] -> Bool
existe _ [] = False
existe x (y:ys)
  | x == y    = True
  | otherwise = existe x ys

--- Función auxiliar para buscar la moda en una lista de tuplas (valor, frecuencia)
buscarModa :: [(Float, Int)] -> Float
buscarModa [] = 0
buscarModa [(x, _)] = x
buscarModa ((x1, f1):(x2, f2):xs)
  | f1 >= f2  = buscarModa ((x1, f1):xs)
  | otherwise = buscarModa ((x2, f2):xs)

-- Función para calcular la moda
-- Retorna la moda, es decir el valor que más se repite en la lista
calcularModa :: [Float] -> Float
calcularModa [] = 0
calcularModa xs = buscarModa (frecuencias xs)

-- Función para calcular la media 
-- Retorna la media (suma de todos los valores divido entre el total de valores)
calcularMedia :: [Float] -> Float
calcularMedia [] = 0
calcularMedia valores = sum valores / fromIntegral (length valores)

-- Función para calcular la mediana
-- Retorna la mediana, es decir el valor que se encuentra en la posición central de la lista ordenada
calcularMediana :: [Float] -> Float
calcularMediana [] = 0
calcularMediana xs = 
  let ordenados = quicksort xs
      n = length ordenados
      mitad = n `div` 2
  in if even n
     then (ordenados !! (mitad - 1) + ordenados !! mitad) / 2
     else ordenados !! mitad

-- Función para aplicar el método seleccionado
aplicarMetodo :: MetodoCompletacion -> [Float] -> Float
aplicarMetodo Media xs = calcularMedia xs
aplicarMetodo Mediana xs = calcularMediana xs
aplicarMetodo Moda xs = calcularModa xs

-- Función para completar las cantidades
-- Filtra las ventas cuyas cantidades sean 0 y le asigna una cantidad utilizando el método seleccionado
-- Retorna una tupla con la lista de Ventas y una Lista con las Ventas que fueron alteradas
completarCantidad :: MetodoCompletacion -> [Venta] -> ([Venta], [VentaId])
completarCantidad metodo ventas =
  let cantidadesValidas = map (fromIntegral . cantidad) $ filter (\venta -> cantidad venta /= 0) ventas
      valorCalculado = aplicarMetodo metodo cantidadesValidas
      cantidadFinal = round valorCalculado
      (ventasActualizadas, idsAlterados) = foldr procesarVenta ([], []) ventas
      procesarVenta venta (ventasAcumuladas, idsAcumulados) =
        if cantidad venta == 0
        then (venta { cantidad = cantidadFinal } : ventasAcumuladas, venta_id venta : idsAcumulados)
        else (venta : ventasAcumuladas, idsAcumulados)
  in (ventasActualizadas, idsAlterados)

-- Función para completar el precio de ventas
-- Filtra las ventas cuyas cantidades sean 0 y le asigna un precio utilizando el método seleccionado
-- Retorna una tupla con la lista de Ventas y una lista con las Ventas que fueron alteradas
completarPrecio :: MetodoCompletacion -> [Venta] -> ([Venta], [VentaId])
completarPrecio metodo ventas =
  let preciosValidos = map precio_unitario $ filter (\venta -> precio_unitario venta /= 0) ventas
      precioCalculado = aplicarMetodo metodo preciosValidos
      (ventasActualizadas, idsAlterados) = foldr procesarVenta ([], []) ventas
      procesarVenta venta (ventasAcumuladas, idsAcumulados) =
        if precio_unitario venta == 0
        then (venta { precio_unitario = precioCalculado } : ventasAcumuladas, venta_id venta : idsAcumulados)
        else (venta : ventasAcumuladas, idsAcumulados)
  in (ventasActualizadas, idsAlterados)

-- Función para eliminar las ventas duplicadas
-- Recibe la lista de ventas y retorna una tupla, una lista tiene las Ventas y la otra las Ventas que fueron eliminadas
-- Las Ventas eliminadas son aquellas que tengan el mismo ID
eliminarDuplicados :: [Venta] -> ([Venta], [VentaId])
eliminarDuplicados ventas =
  let ventasUnicas = nubBy (\venta1 venta2 -> venta_id venta1 == venta_id venta2) ventas
      cantidadOriginal = length ventas
      cantidadUnica = length ventasUnicas
      cantidadDuplicados = cantidadOriginal - cantidadUnica
      idsEliminados = if cantidadDuplicados > 0
                      then obtenerIdsDuplicados ventas ventasUnicas
                      else []
  in (ventasUnicas, idsEliminados)

-- Función auxiliar para obtener los IDs de las ventas duplicadas eliminadas
obtenerIdsDuplicados :: [Venta] -> [Venta] -> [VentaId]
obtenerIdsDuplicados originales unicas =
  let idsOriginales = map venta_id originales
      idsUnicos = map venta_id unicas
      contarOcurrencias id lista = length (filter (== id) lista)
      idsDuplicados = [id | id <- idsOriginales, contarOcurrencias id idsOriginales > 1]
  in nubBy (==) idsDuplicados

-- Función para mostra el ID de las Ventas.
mostrarIds :: [VentaId] -> IO ()
mostrarIds ids = mapM_ (\idVenta -> putStrLn $ "  - Venta ID: " ++ show idVenta) ids

-- Función para mostrar los datos completos de una venta
mostrarVenta :: Venta -> IO ()
mostrarVenta venta = do
  putStrLn $ "  ID: " ++ show (venta_id venta)
  putStrLn $ "    Fecha: " ++ fecha venta
  putStrLn $ "    Producto ID: " ++ show (producto_id venta)
  putStrLn $ "    Producto: " ++ producto_nombre venta
  putStrLn $ "    Categoría: " ++ categoria venta
  putStrLn $ "    Cantidad: " ++ show (cantidad venta)
  putStrLn $ "    Precio Unitario: " ++ show (precio_unitario venta)
  putStrLn $ "    Total: " ++ show (total venta)
  putStrLn ""

-- Función para buscar ventas por sus IDs
buscarVentasPorIds :: [VentaId] -> [Venta] -> [Venta]
buscarVentasPorIds ids ventas = filter (\venta -> venta_id venta `elem` ids) ventas

-- Función para mostrar todas las ventas modificadas
mostrarVentasModificadas :: [VentaId] -> [Venta] -> IO ()
mostrarVentasModificadas ids ventas = do
  let ventasModificadas = buscarVentasPorIds ids ventas
  mapM_ mostrarVenta ventasModificadas

-- Función para mostrar el menú de selección de método a utilizar
mostrarMenuMetodo :: IO MetodoCompletacion
mostrarMenuMetodo = do
  putStrLn "\n--- Seleccione el método de completación de datos ---"
  putStrLn "1. Media (promedio)"
  putStrLn "2. Mediana (valor central)"
  putStrLn "3. Moda (valor más frecuente)"
  putStr "Seleccione una opción (1-3): "
  opcion <- getLine
  case opcion of
    "1" -> do
      putStrLn "Método seleccionado: Media"
      return Media
    "2" -> do
      putStrLn "Método seleccionado: Mediana"
      return Mediana
    "3" -> do
      putStrLn "Método seleccionado: Moda"
      return Moda
    _ -> do
      putStrLn "Opción inválida"
      mostrarMenuMetodo

-- Función para mostrar el menú del procesamiento de datos
menuProcesadoDatos :: Ventas -> IO Ventas
menuProcesadoDatos (Ventas ventas) = do
  if null ventas
    then do
      putStrLn "\nNo hay ventas cargadas. Importe datos primero."
      return (Ventas [])
    else do
      putStrLn "\n--- Procesamiento de Datos ---"
      
      metodo <- mostrarMenuMetodo
      
      let (ventasConCantidad, idsAlteradosCantidad) = completarCantidad metodo ventas
      if null idsAlteradosCantidad
        then putStrLn "No se encontraron cantidades faltantes."
        else do
          putStrLn "Cantidades completadas con la media en las siguientes ventas:"
          mostrarVentasModificadas idsAlteradosCantidad ventasConCantidad
      
      let (ventasConPrecio, idsAlteradosPrecios) = completarPrecio metodo ventasConCantidad
      if null idsAlteradosPrecios
        then putStrLn "No se encontraron precios faltantes."
        else do
          putStrLn "Precios completados con la media en las siguientes ventas:"
          mostrarVentasModificadas idsAlteradosPrecios ventasConPrecio
      
      let (ventasFinales, idsDuplicadosEliminados) = eliminarDuplicados ventasConPrecio
      if null idsDuplicadosEliminados
        then putStrLn "No se encontraron duplicados."
        else do
          putStrLn "Ventas duplicadas eliminadas:"
          mostrarIds idsDuplicadosEliminados
      
      putStrLn $ "\nTotal de ventas procesadas: " ++ show (length ventasFinales)
      return (Ventas ventasFinales)