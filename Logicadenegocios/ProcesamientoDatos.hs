module Logicadenegocios.ProcesamientoDatos (menuProcesadoDatos) where
import Logicadenegocios.Estructuras
import Data.List (nubBy)

-- Función para calcular la media 
-- Retorna la media (suma de todos los valores divido entre el total de valores)
calcularMedia :: [Float] -> Float
calcularMedia [] = 0
calcularMedia valores = sum valores / fromIntegral (length valores)


-- Función para completar las cantidades
-- Filtra las ventas cuyas cantidades sean 0 y le asigna una cantidad utilizando la media
-- Retorna una tupla con la lista de Ventas y una Lista con las Ventas que fueron alteradas
completarCantidad :: [Venta] -> ([Venta], [VentaId])
completarCantidad ventas =
  let cantidadesValidas = map (fromIntegral . cantidad) $ filter (\venta -> cantidad venta /= 0) ventas
      mediaCantidades = round $ calcularMedia cantidadesValidas
      (ventasActualizadas, idsAlterados) = foldr procesarVenta ([], []) ventas
      procesarVenta venta (ventasAcumuladas, idsAcumulados) =
        if cantidad venta == 0
        then (venta { cantidad = mediaCantidades } : ventasAcumuladas, venta_id venta : idsAcumulados)
        else (venta : ventasAcumuladas, idsAcumulados)
  in (ventasActualizadas, idsAlterados)

-- Función para completar el precio de ventas
-- Filtra las ventas cuyas cantidades sean 0 y le asigna un precio utilizando la media
-- Retorna una tupla con la lista de Ventas y una lista con las Ventas que fueron alteradas
completarPrecio :: [Venta] -> ([Venta], [VentaId])
completarPrecio ventas =
  let preciosValidos = map precio_unitario $ filter (\venta -> precio_unitario venta /= 0) ventas
      mediaPrecios = calcularMedia preciosValidos
      (ventasActualizadas, idsAlterados) = foldr procesarVenta ([], []) ventas
      procesarVenta venta (ventasAcumuladas, idsAcumulados) =
        if precio_unitario venta == 0
        then (venta { precio_unitario = mediaPrecios } : ventasAcumuladas, venta_id venta : idsAcumulados)
        else (venta : ventasAcumuladas, idsAcumulados)
  in (ventasActualizadas, idsAlterados)

-- Función para eliminar las ventas duplicadas
-- Recibe la lista de ventas y retorna una tupla, una lista tiene las Ventas y la otra las Ventas que fueron eliminadas
-- Las Ventas eliminadas son aquellas que tengan el mismo ID
eliminarDuplicados :: [Venta] -> ([Venta], [VentaId])
eliminarDuplicados ventas =
  let ventasUnicas = nubBy (\venta1 venta2 -> venta_id venta1 == venta_id venta2) ventas
      idsOriginales = map venta_id ventas
      idsUnicos = map venta_id ventasUnicas
      idsEliminados = filter (`notElem` idsUnicos) idsOriginales
  in (ventasUnicas, idsEliminados)

-- Función para mostra el ID de las Ventas.
mostrarIds :: [VentaId] -> IO ()
mostrarIds ids = mapM_ (\idVenta -> putStrLn $ "  - Venta ID: " ++ show idVenta) ids

-- Función para mostrar el menú del procesamiento de datos
-- Realiza los procesamientos respectivos sin necesidad de seleccionar ninguna opcion.
-- #Preguntar al profe si debe ser en opciones
menuProcesadoDatos :: Ventas -> IO Ventas
menuProcesadoDatos (Ventas ventas) = do
  if null ventas
    then do
      putStrLn "\nNo hay ventas cargadas. Importe datos primero."
      return (Ventas [])
    else do
      putStrLn "\n--- Procesamiento de Datos ---"
      
      let (ventasConCantidad, idsAlteradosCantidad) = completarCantidad ventas
      if null idsAlteradosCantidad
        then putStrLn "No se encontraron cantidades faltantes."
        else do
          putStrLn "Cantidades completadas con la media en las siguientes ventas:"
          mostrarIds idsAlteradosCantidad
      
      let (ventasConPrecio, idsAlteradosPrecios) = completarPrecio ventasConCantidad
      if null idsAlteradosPrecios
        then putStrLn "No se encontraron precios faltantes."
        else do
          putStrLn "Precios completados con la media en las siguientes ventas:"
          mostrarIds idsAlteradosPrecios
      
      let (ventasFinales, idsDuplicadosEliminados) = eliminarDuplicados ventasConPrecio
      if null idsDuplicadosEliminados
        then putStrLn "No se encontraron duplicados."
        else do
          putStrLn "Ventas duplicadas eliminadas:"
          mostrarIds idsDuplicadosEliminados
      
      putStrLn $ "\nTotal de ventas procesadas: " ++ show (length ventasFinales)
      return (Ventas ventasFinales)