module Logicadenegocios.Estadisticas where
import Logicadenegocios.Estructuras
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Data.List (sortBy)
import Data.Ord (comparing)





todasLasCategoriasAux :: ([Categoria], [Venta]) -> [Categoria]
todasLasCategoriasAux (categorias, []) = categorias
todasLasCategoriasAux (categorias, x:xs) =
  if categoria x `elem` categorias
     then todasLasCategoriasAux (categorias, xs)
     else todasLasCategoriasAux (categoria x : categorias, xs)

todasLasCategorias :: [Venta] -> [Categoria]
todasLasCategorias ventas = todasLasCategoriasAux ([], ventas) 
  

totalVentasCategoriaAux :: (Categoria, [Venta], Float) -> Float
totalVentasCategoriaAux (_, [], res) = res
totalVentasCategoriaAux (cat, x:xs, res) =
  if categoria x == cat
     then totalVentasCategoriaAux (cat, xs, res + total x)
     else totalVentasCategoriaAux (cat, xs, res)

totalVentasCategoria :: Categoria -> [Venta] -> Float
totalVentasCategoria categoria ventas = totalVentasCategoriaAux (categoria, ventas, 0.0)


pasada :: ([Categoria], [Venta]) -> [Categoria]
pasada ([], _) = []
pasada ([x], _) = [x]
pasada (x:y:xs, ventas) =
  if totalVentasCategoria x ventas < totalVentasCategoria y ventas
     then y : pasada (x:xs, ventas)
     else x : pasada (y:xs, ventas)


ordenarCategoriasAux :: ([Categoria], [Venta]) -> [Categoria]
ordenarCategoriasAux (categorias, ventas) = 
    let 
        ordenado = pasada (categorias, ventas)
    in
        if ordenado == categorias
            then ordenado
            else ordenarCategoriasAux(ordenado, ventas)

ordenarCategorias :: ([Categoria],[Venta]) -> [Categoria]
ordenarCategorias (categorias, ventas) =
    sortBy (flip (comparing (\cat -> totalVentasCategoria cat ventas))) categorias

obtenerCantidadCategorias :: [Venta] -> Int 
obtenerCantidadCategorias ventas = length (todasLasCategorias ventas)

topCincoCategorias :: [Venta] -> [Categoria]
topCincoCategorias ventas =
    let 
        categorias = todasLasCategorias ventas
        categoriasOrdenadas = ordenarCategorias (categorias, ventas)
    in 
        take 5 categoriasOrdenadas




imprimirCategoriasAux :: ([Categoria], [Venta], Int) -> IO ()
imprimirCategoriasAux ([], _, _) = return ()
imprimirCategoriasAux (cat:xs, ventas, n) = do
    putStrLn (show n ++ "- " ++ cat ++ ": " ++ show (totalVentasCategoria cat ventas))
    imprimirCategoriasAux (xs, ventas, n + 1)

imprimirTopCincoCategorias :: [Venta] -> IO ()
imprimirTopCincoCategorias ventas = do
    putStrLn "Top 5 Categorías con más ventas"
    imprimirCategoriasAux (topCincoCategorias ventas, ventas, 1)

todosLosProductosAux :: ([ProductoId], [Venta]) -> [ProductoId]
todosLosProductosAux (productos, []) = productos
todosLosProductosAux (productos, x:xs) =
  if producto_id x `elem` productos
     then todosLosProductosAux (productos, xs)
     else todosLosProductosAux (producto_id x : productos, xs)

todosLosProductos :: [Venta] -> [ProductoId]
todosLosProductos ventas = todosLosProductosAux ([], ventas) 

nombreProducto :: (ProductoId, [Venta]) -> String
nombreProducto (_, []) = "Producto no encontrado"
nombreProducto (id, x:xs) =
    if producto_id x == id
        then producto_nombre x
        else nombreProducto (id, xs)


cantVentasProductoAux :: (ProductoId, [Venta], Int) -> Int
cantVentasProductoAux (_, [], res) = res
cantVentasProductoAux (prod, x:xs, res) =
  if producto_id x == prod
     then cantVentasProductoAux (prod, xs, res + cantidad x)
     else cantVentasProductoAux (prod, xs, res)

cantVentasProducto :: (ProductoId , [Venta]) -> Int
cantVentasProducto (id, ventas) = cantVentasProductoAux (id, ventas, 0)

mayorVentaProducto :: ([Venta], [ProductoId], ProductoId, Int) -> ProductoId
mayorVentaProducto (_, [], mayor, _) = mayor
mayorVentaProducto (ventas, x:xs, mayor, cantMayor) =
    let 
        cantVentas = cantVentasProducto (x, ventas)
    in
        if cantVentas > cantMayor
            then mayorVentaProducto (ventas, xs, x, cantVentas)
            else mayorVentaProducto (ventas, xs, mayor, cantMayor)

imprimirMayorVentaProducto :: [Venta] -> IO()
imprimirMayorVentaProducto ventas = 
    let 
        productos = todosLosProductos ventas
        mayorProducto = mayorVentaProducto (ventas, productos, 0,0)
        cantidadProducto = cantVentasProducto (mayorProducto, ventas)
        nombreMayor = nombreProducto (mayorProducto, ventas)
    in do
        putStrLn "Producto más vendido"
        if cantidadProducto == 1 then putStrLn (nombreMayor ++ ": " ++ show cantidadProducto ++ " venta")
        else putStrLn (nombreMayor ++ ": " ++ show cantidadProducto ++ " ventas")
    

menuEstadisticas :: Ventas -> IO ()
menuEstadisticas (Ventas ventas) = do
    if null ventas
       then putStrLn "\nNo hay ventas cargadas. Importe datos primero."
       else do
           putStrLn "\n--- Estadísticas ---"
           putStrLn "A - Top 5 Categorías con más ventas"
           putStrLn "B - Producto más vendido"
           putStrLn "C - Categoría con menor participación"
           putStrLn "D - Resumen General"
           putStrLn "R- Regresar"
           putStrLn "Seleccione una opción: "
           opcion <- getLine
           case opcion of 
               "A" -> do
                   imprimirTopCincoCategorias ventas
                   menuEstadisticas (Ventas ventas)
               "B" -> do
                   imprimirMayorVentaProducto ventas
                   menuEstadisticas (Ventas ventas)
               "R" -> do
                   putStrLn "Regresando al menú principal..."
               _ -> do
                   putStrLn "Opción no válida, intente de nuevo."
                   menuEstadisticas (Ventas ventas)


