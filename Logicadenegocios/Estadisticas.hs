module Logicadenegocios.Estadisticas where
import Logicadenegocios.Estructuras
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Data.List (sortBy, isSuffixOf)
import Data.Ord (comparing)
import System.Directory (doesFileExist)
import System.IO (withFile, IOMode(WriteMode), hSetEncoding, hPutStr, utf8)




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


parsearContenidoTopCincoCategorias :: ([Categoria], [Venta], String, Int ) -> String
parsearContenidoTopCincoCategorias ([], _, contenido, _) = contenido
parsearContenidoTopCincoCategorias (x:xs, ventas, contenido, n) = 
    let
        total = totalVentasCategoria x ventas
        linea = show n ++ "," ++ x ++ "," ++ show total ++ "\n"
        nuevoCont = contenido ++ linea
    in 
        parsearContenidoTopCincoCategorias (xs, ventas, nuevoCont, n+1)

guardarTopCincoCategorias :: ([Categoria], [Venta]) -> IO()
guardarTopCincoCategorias (categorias, ventas) = 
    let contenido = parsearContenidoTopCincoCategorias (categorias, ventas, "Posicion,Categoria,Total\n" ,1)
    in do
        archivo <- preguntaGuardar  
        withFile archivo WriteMode $ \handle -> do
            hSetEncoding handle utf8
            hPutStr handle contenido
        putStrLn "Se ha guardado la estadistica correctamente"

imprimirCategoriasAux :: ([Categoria], [Venta], Int) -> IO ()
imprimirCategoriasAux ([], _, _) = return ()
imprimirCategoriasAux (cat:xs, ventas, n) = do
    putStrLn (show n ++ "- " ++ cat ++ ": " ++ show (totalVentasCategoria cat ventas))
    imprimirCategoriasAux (xs, ventas, n + 1)

imprimirTopCincoCategorias :: [Venta] -> IO ()
imprimirTopCincoCategorias ventas = 
    let
        topCinco = topCincoCategorias ventas
    in do
        putStrLn "Top 5 Categorías con más ventas"
        imprimirCategoriasAux (topCinco, ventas, 1)
        guardarTopCincoCategorias (topCinco, ventas)

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



mayorVentaProducto :: ([Venta], [ProductoId], ProductoId, Int) -> ProductoId
mayorVentaProducto (_, [], mayor, _) = mayor
mayorVentaProducto (ventas, x:xs, mayor, cantMayor) =
    let 
        cantVentas = cantVentasProducto (x, ventas)
    in
        if cantVentas > cantMayor
            then mayorVentaProducto (ventas, xs, x, cantVentas)
            else mayorVentaProducto (ventas, xs, mayor, cantMayor)

guardarMayorVentaProducto :: (String, Int) -> IO()
guardarMayorVentaProducto(nombre, cantidad) = 
    let contenido = "Nombre,Cantidad\n" ++ nombre ++ "," ++ show cantidad
    in do
        archivo <- preguntaGuardar  
        withFile archivo WriteMode $ \handle -> do
            hSetEncoding handle utf8
            hPutStr handle contenido
        putStrLn "Se ha guardado la estadistica correctamente"


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
        guardarMayorVentaProducto (nombreMayor, cantidadProducto)
    

cantVentasProductoAux :: (ProductoId, [Venta], Int) -> Int
cantVentasProductoAux (_, [], res) = res
cantVentasProductoAux (prod, x:xs, res) =
  if producto_id x == prod
     then cantVentasProductoAux (prod, xs, res + cantidad x)
     else cantVentasProductoAux (prod, xs, res)

cantVentasProducto :: (ProductoId , [Venta]) -> Int
cantVentasProducto (id, ventas) = cantVentasProductoAux (id, ventas, 0)

cantVentasCategoriaAux :: (Categoria, [Venta], Int) -> Int
cantVentasCategoriaAux (_, [], res) = res
cantVentasCategoriaAux (cat, x:xs, res) =
  if categoria x == cat
     then cantVentasCategoriaAux (cat, xs, res + cantidad x)
     else cantVentasCategoriaAux (cat, xs, res)

cantVentasCategoria :: (Categoria , [Venta]) -> Int
cantVentasCategoria (cat, ventas) = cantVentasCategoriaAux (cat, ventas, 0)

menorCantVentaCategoria :: ([Venta], [Categoria], Categoria, Int) -> Categoria
menorCantVentaCategoria (_, [], menor, _) = menor
menorCantVentaCategoria (ventas, x:xs, menor, cantMenor) =
    let 
        cantVentas = cantVentasCategoria (x, ventas)
    in
        if cantVentas < cantMenor
            then menorCantVentaCategoria (ventas, xs, x, cantVentas)
            else menorCantVentaCategoria (ventas, xs, menor, cantMenor)


nombreCategoria :: (Categoria, [Venta]) -> String
nombreCategoria (_, []) = "Categoría no encontrada"
nombreCategoria (cat, x:xs) =
    if categoria x == cat
        then categoria x
        else nombreCategoria (cat, xs)

guardarMenorCantVentaCategoria :: (String, Int) -> IO()
guardarMenorCantVentaCategoria (nombre, cantidad) = 
    let contenido = "Nombre,Cantidad\n" ++ nombre ++ "," ++ show cantidad
    in do
        archivo <- preguntaGuardar  
        withFile archivo WriteMode $ \handle -> do
            hSetEncoding handle utf8
            hPutStr handle contenido
        putStrLn "Se ha guardado la estadistica correctamente"


imprimirMenorCantVentaCategoria :: [Venta] -> IO()
imprimirMenorCantVentaCategoria ventas = 
    let 
        categorias = todasLasCategorias ventas
        menorCategoria = menorCantVentaCategoria (ventas, categorias, "",999999)
        cantidadCategoria = cantVentasCategoria (menorCategoria, ventas)
        nombreMenor = nombreCategoria (menorCategoria, ventas)
    in do
        putStrLn "Categoría con menor participación"
        if cantidadCategoria == 1 then putStrLn (nombreMenor ++ ": " ++ show cantidadCategoria ++ " venta")
        else putStrLn (nombreMenor ++ ": " ++ show cantidadCategoria ++ " ventas")
        guardarMenorCantVentaCategoria (nombreMenor, cantidadCategoria)


imprimirCantVentasCategoriasAux  :: ([Categoria], [Venta]) -> IO ()
imprimirCantVentasCategoriasAux ([], _) = return ()
imprimirCantVentasCategoriasAux (cat:xs, ventas) = do
    putStrLn (cat ++ ": " ++ show (cantVentasCategoria (cat, ventas)))
    imprimirCantVentasCategoriasAux (xs, ventas)

parsearCantVentasCategorias :: ([Categoria], [Venta], String) -> String
parsearCantVentasCategorias ([], _, contenido) = contenido
parsearCantVentasCategorias (x:xs, ventas, contenido) =
    let
        cantidad = cantVentasCategoria (x, ventas)
        linea = "1," ++ x ++ "," ++ show cantidad ++ "\n"
        nuevoCont = contenido ++ linea
    in parsearCantVentasCategorias (xs, ventas, nuevoCont)

        

imprimirCantVentasCategorias :: [Venta] -> IO()
imprimirCantVentasCategorias ventas =
    let
        categorias = todasLasCategorias ventas
    in do
        putStrLn "1- Cantidad de Ventas por Categoría"
        imprimirCantVentasCategoriasAux (categorias, ventas)
       


ventaMasAltaAux :: ([Venta], Venta, Float) -> Venta
ventaMasAltaAux ([], mayor, _) = mayor
ventaMasAltaAux (x:xs, mayor, cantMayor) =
    if total x > cantMayor 
        then ventaMasAltaAux (xs, x, total x) 
        else ventaMasAltaAux (xs, mayor, cantMayor)

ventaMasAlta :: [Venta] -> Venta
ventaMasAlta (x:xs) = ventaMasAltaAux (xs, x, total x)

ventaMasBajaAux :: ([Venta], Venta, Float) -> Venta
ventaMasBajaAux ([], menor, _) = menor
ventaMasBajaAux (x:xs, menor, cantMenor) =
    if total x < cantMenor
        then ventaMasBajaAux (xs, x, total x)
        else ventaMasBajaAux (xs, menor, cantMenor)

ventaMasBaja :: [Venta] -> Venta
ventaMasBaja (x:xs) = ventaMasBajaAux (xs, x, total x)

imprimirVenta :: Venta -> IO()
imprimirVenta venta = do 
        putStrLn ("Venta " ++ show (venta_id venta) )
        putStrLn ("Producto: " ++ show (producto_nombre venta))
        putStrLn ("Total: " ++ show (total venta))

preguntaGuardar :: IO String
preguntaGuardar = do
    putStrLn "Ingrese el nombre del archivo (CSV) donde desea guardar la estadística:"
    nombre <- getLine
    if null nombre
        then do
            putStrLn "El nombre no puede estar vacío."
            preguntaGuardar
        else do
            let nombreCsv = if ".csv" `isSuffixOf` nombre
                            then nombre
                            else nombre ++ ".csv"
            return nombreCsv


parsearVentaAltaBaja :: [Venta] -> String
parsearVentaAltaBaja ventas = 
    let 
        ventaAlta = ventaMasAlta ventas
        ventaBaja = ventaMasBaja ventas
        contenidoAlta = "Reporte,Venta,Producto,Cantidad\n2," ++ show (venta_id ventaAlta) ++ "," ++ producto_nombre ventaAlta ++ "," ++ show (total ventaAlta) ++ "\n"
        contenidoBaja = "3,"++ show (venta_id ventaBaja) ++ "," ++ producto_nombre ventaBaja ++ "," ++ show (total ventaBaja) ++ "\n"
        contenido = contenidoAlta ++ contenidoBaja
    in
        contenido


imprimirVentaAltaBaja :: [Venta] -> IO()
imprimirVentaAltaBaja ventas = 
    let
        ventaAlta = ventaMasAlta ventas
        ventaBaja = ventaMasBaja ventas
    in do
        putStrLn "2- Venta más alta:"
        imprimirVenta ventaAlta
        putStrLn "3- Venta más baja:"
        imprimirVenta ventaBaja
        
productosPorCategoriaAux :: ([Venta], Categoria, [ProductoId]) -> [ProductoId]
productosPorCategoriaAux ([], _, productos) = productos
productosPorCategoriaAux (x:xs, cat, productos) =
    if categoria x == cat
        then 
            if producto_id x `elem` productos
                then productosPorCategoriaAux (xs, cat, productos)
                else productosPorCategoriaAux (xs, cat, producto_id x : productos)
        else productosPorCategoriaAux (xs, cat, productos)



productosPorCategoria :: ([Venta], Categoria) -> [ProductoId]
productosPorCategoria (ventas, cat) = productosPorCategoriaAux (ventas, cat, [])

cantProductosCategoria :: ([Venta], Categoria) -> Int
cantProductosCategoria (ventas, cat) = length (productosPorCategoria(ventas, cat))

categoriaMasVariadaAux :: ([Venta], [Categoria], Categoria, Int) -> Categoria
categoriaMasVariadaAux (_, [], mayor, _) = mayor
categoriaMasVariadaAux (ventas, x:xs, mayor, cantMayor) =
    let
        cantProductos = cantProductosCategoria (ventas, x)
    in
        if cantProductos > cantMayor
            then categoriaMasVariadaAux (ventas, xs, x, cantProductos)
            else categoriaMasVariadaAux (ventas, xs, mayor, cantMayor)



categoriaMasVariada :: [Venta] -> Categoria
categoriaMasVariada ventas =
    let 
        categorias = todasLasCategorias ventas
        primera = head categorias
        cantInicial = cantProductosCategoria (ventas, primera)
    in
        categoriaMasVariadaAux (ventas, categorias, primera, cantInicial)


parsearCategoriaMasVariada :: [Venta] -> String
parsearCategoriaMasVariada ventas =
    let 
        categoria = categoriaMasVariada ventas
        cantidad = cantProductosCategoria (ventas, categoria)
        contenido = "Reporte,Categoria,Cantidad\n4," ++ categoria ++ "," ++ show cantidad
    in
        contenido



imprimirCategoriaMasVariadaAux :: ([Venta], Categoria) -> IO()
imprimirCategoriaMasVariadaAux (ventas, categoria) = 
    let 
        cantProductos = cantProductosCategoria (ventas, categoria)
    in
        if cantProductos == 1 then putStrLn (categoria ++ ": " ++ show cantProductos ++ " producto")
        else putStrLn (categoria ++ ": " ++ show cantProductos ++ " productos")
        
imprimirCategoriaMasVariada :: [Venta] -> IO()
imprimirCategoriaMasVariada ventas = 
    let 
        categoria = categoriaMasVariada ventas 
    in do
        putStrLn "4- Categoría más variada"
        imprimirCategoriaMasVariadaAux (ventas, categoria)

guardarResumenGeneral :: [Venta] -> IO()
guardarResumenGeneral ventas = 
    let
        categorias = todasLasCategorias ventas
        contenido1 = parsearCantVentasCategorias(categorias, ventas, "Reporte,Categoria,Cantidad\n")
        contenido2 = parsearVentaAltaBaja ventas
        contenido3 = parsearCategoriaMasVariada ventas
        contenido = contenido1 ++ contenido2 ++ contenido3
    in do
        archivo <- preguntaGuardar  
        withFile archivo WriteMode $ \handle -> do
            hSetEncoding handle utf8
            hPutStr handle contenido
        putStrLn "Se ha guardado la estadística correctamente"

     

imprimirResumenGeneral :: [Venta] -> IO()
imprimirResumenGeneral ventas = do 
    putStrLn "-----Resumen General-----"
    imprimirCantVentasCategorias ventas
    imprimirVentaAltaBaja ventas
    imprimirCategoriaMasVariada ventas
    guardarResumenGeneral ventas
    


guardarEnArchivo :: (String, String) -> IO()
guardarEnArchivo (archivo, texto) = do
    writeFile archivo texto
    putStrLn "Archivo creado o sobrescrito con éxito."

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
                "C" -> do
                    imprimirMenorCantVentaCategoria ventas
                    menuEstadisticas (Ventas ventas)
                "D" -> do
                    imprimirResumenGeneral ventas
                    menuEstadisticas(Ventas ventas)
                "R" -> do
                   putStrLn "Regresando al menú principal..."
                _ -> do
                   putStrLn "Opción no válida, intente de nuevo."
                   menuEstadisticas (Ventas ventas)


