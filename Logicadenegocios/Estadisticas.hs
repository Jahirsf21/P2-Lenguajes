module Logicadenegocios.Estadisticas where
import Logicadenegocios.Estructuras
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Data.List (sortBy, isSuffixOf)
import Data.Ord (comparing)
import System.Directory (doesFileExist)
import System.IO (withFile, IOMode(WriteMode), hSetEncoding, hPutStr, utf8)



--Funcion auxiliar que recorre recursivamente la lista de ventas, recibe una lista de categorias y una lista de ventas
--y va agregando en la lista categorias todas las categorias presentes, sin repetir ninguna
--en la primera llamada la lista categorias está vacia y la lista de ventas se descompone realizando x:xs, 
--donde x es la primera venta presente en esa llamada y xs el resto de la lista de ventas
--Termina cuando la lista de ventas esté vacía, y devuelve la lista categorias con todas las categorias
todasLasCategoriasAux :: ([Categoria], [Venta]) -> [Categoria]
todasLasCategoriasAux (categorias, []) = categorias
todasLasCategoriasAux (categorias, x:xs) =
  if categoria x `elem` categorias
     then todasLasCategoriasAux (categorias, xs)
     else todasLasCategoriasAux (categoria x : categorias, xs)

--funcion principal que devuelve una lista con todas las categorias, recibe la lista de ventas
--Llama a la funcion auxiliar que realiza toda la logica y devuelve el resultado de esa funcion 
--Se envía una lista vacía para inicializar la lista de categorias
todasLasCategorias :: [Venta] -> [Categoria]
todasLasCategorias ventas = todasLasCategoriasAux ([], ventas) 
  
--funcion auxiliar que recorre recursivamente la lista de ventas, recibe una categoria, la lista de ventas y un numero flotante
--Suma cada total de ventas que coincidan con la categoría que recibe
--en la primera llamada, el resultado es 0.0, para ir sumando los totales en res, se descompone la lista de ventas realizando x:xs
--donde x es la primera venta presente en esa llamada y xs el resto de la lista de ventas
--Termina cuando la lista de ventas esté vacía, y devuelve res que es la suma de todas las ventas de esa categoria
totalVentasCategoriaAux :: (Categoria, [Venta], Float) -> Float
totalVentasCategoriaAux (_, [], res) = res
totalVentasCategoriaAux (cat, x:xs, res) =
  if categoria x == cat
     then totalVentasCategoriaAux (cat, xs, res + total x)
     else totalVentasCategoriaAux (cat, xs, res)

--funcion principal que devuelve el total de ventas de una categoria, recibe una categoria y la lista de ventas
--Llama a la funcion auxiliar que realiza toda la logica y devuelve el resultado de esa funcion 
--Se envía un 0.0  para inicializar el sumador del total
totalVentasCategoria :: Categoria -> [Venta] -> Float
totalVentasCategoria categoria ventas = totalVentasCategoriaAux (categoria, ventas, 0.0)

--funcion que ordena las categorias de mayor a menor, recibe una lista con todas las categorias y otra con las ventas
--los ordena con el criterio de comparación total en ventas
--utiliza funciones de orden superior para realizar el ordenamiento
--sortBy ordena la lista usando una funcion de comparación
--comparing convierte la funcion lambda, que devuelve el total de ventas por cada categoria, en una funcion de comparación
--flip invierte el orden de la comparación, ya que originalmente el sortBy ordena de menor a mayor
ordenarCategorias :: ([Categoria],[Venta]) -> [Categoria]
ordenarCategorias (categorias, ventas) =
    sortBy (flip (comparing (\cat -> totalVentasCategoria cat ventas))) categorias

---funcion que devuelve la cantidad de categorias que existen en las ventas
--recibe la lista de las ventas y devuelve la longitud de la lista con todas las categorias
obtenerCantidadCategorias :: [Venta] -> Int 
obtenerCantidadCategorias ventas = length (todasLasCategorias ventas)

--funcion que devuelve el top 5 de categorias, si existen 5 o más, si existen menos categorias, devuelve la cantidad que existe
--recibe la lista de ventas, accede a la funcion que devuelve todas las categorias y los ordena de mayor a menor
--take  es una funcion de orden superior y en este caso devuelve los primeros 5 elementos, 
--o los n elementos que existen si tuviera menos de 5 elementos
topCincoCategorias :: [Venta] -> [Categoria]
topCincoCategorias ventas =
    let 
        categorias = todasLasCategorias ventas
        categoriasOrdenadas = ordenarCategorias (categorias, ventas)
    in 
        take 5 categoriasOrdenadas

--funcion que parsea el top 5 de categorias para guardarlo en un archivo
---recibe una lista de categorias, y forma un string con el contenido formateado para un csv
--recorre la lista de categorias recursivamente, descomponiendolo con x:xs
parsearContenidoTopCincoCategorias :: ([Categoria], [Venta], String, Int ) -> String
parsearContenidoTopCincoCategorias ([], _, contenido, _) = contenido
parsearContenidoTopCincoCategorias (x:xs, ventas, contenido, n) = 
    let
        total = totalVentasCategoria x ventas
        linea = show n ++ "," ++ x ++ "," ++ show total ++ "\n"
        nuevoCont = contenido ++ linea
    in 
        parsearContenidoTopCincoCategorias (xs, ventas, nuevoCont, n+1)

---guarda la información del top 5 de categorias en un archivo, que se pide al usuario
--recibe una lista de categorias y la lista de ventas, parsea el contenido a través de la función correspondiente, 
--y en la función preguntaGuardar se pregunta al usuario en que archivo desea guardar,
--se guarda en ese archivo con codificación UTF-8
guardarTopCincoCategorias :: ([Categoria], [Venta]) -> IO()
guardarTopCincoCategorias (categorias, ventas) = 
    let contenido = parsearContenidoTopCincoCategorias (categorias, ventas, "Posicion,Categoria,Total\n" ,1)
    in do
        archivo <- preguntaGuardar  
        withFile archivo WriteMode $ \handle -> do
            hSetEncoding handle utf8
            hPutStr handle contenido
        putStrLn "Se ha guardado la estadistica correctamente"


--función auxiliar que recorre la lista de categorias e imprime su información
--recibe una lista de categorias , la lista de ventas y un entero
--descompone la lista de categorias con cat:xs y se termina cuando esta lista sea vacía
--el la primera llamada el entero será 1, porque este será la posición en el top de la categoria
--la lista siempre tendrá 5 elementos o menos
imprimirCategoriasAux :: ([Categoria], [Venta], Int) -> IO ()
imprimirCategoriasAux ([], _, _) = return ()
imprimirCategoriasAux (cat:xs, ventas, n) = do
    putStrLn (show n ++ "- " ++ "Categoría: " ++ cat ++ " | Total: " ++ show (totalVentasCategoria cat ventas))
    imprimirCategoriasAux (xs, ventas, n + 1)


--función principal que imprime la estadistica de top 5 categorias, recibe la lista de ventas
-- obtiene el top 5 y a través de la función auxiliar imprime su información
--se aprovecha que se ha obtenido el top 5 y se llama a la función de guardar la información en un archivo
imprimirTopCincoCategorias :: [Venta] -> IO ()
imprimirTopCincoCategorias ventas = 
    let
        topCinco = topCincoCategorias ventas
    in do
        putStrLn "--------- Top 5 Categorías con más ventas ---------"
        imprimirCategoriasAux (topCinco, ventas, 1)
        guardarTopCincoCategorias (topCinco, ventas)

--función auxiliar que devuelve todos los productos, recibe una lista de ProductosId y la lista de ventas
--recorre la lista de ventas, y guarda todos los productos no repetidos en la lista
-- en la primera llamada, la lista de productos está vacia para ir guardando los ProductosId
--finaliza cuando la lista de ventas esté vacía
todosLosProductosAux :: ([ProductoId], [Venta]) -> [ProductoId]
todosLosProductosAux (productos, []) = productos
todosLosProductosAux (productos, x:xs) =
  if producto_id x `elem` productos
     then todosLosProductosAux (productos, xs)
     else todosLosProductosAux (producto_id x : productos, xs)

--función principal que devuelve todos los productos que estén en las ventas, recibe la lista de ventas
--a través de la función auxiliar obtiene la lista de productos
--Se envía una lista vacía para inicializar la lista de productos
todosLosProductos :: [Venta] -> [ProductoId]
todosLosProductos ventas = todosLosProductosAux ([], ventas) 

--funcion que devuelve el nombre del producto correspondiente, recibe un ProductoId y la lista de ventas
--si no lo encuentra, devuelve producto no encontrado
nombreProducto :: (ProductoId, [Venta]) -> String
nombreProducto (_, []) = "Producto no encontrado"
nombreProducto (id, x:xs) =
    if producto_id x == id
        then producto_nombre x
        else nombreProducto (id, xs)


--función que busca el producto con mayor cantidad de ventas
--recibe la lista de ventas, la lista de productos, el producto actual mayor y la cantidad mayor
--recorre recursivamente la lista de productos y actualiza el producto con más ventas
--devuelve el ProductoId del producto con mayor cantidad de ventas
mayorVentaProducto :: ([Venta], [ProductoId], ProductoId, Int) -> ProductoId
mayorVentaProducto (_, [], mayor, _) = mayor
mayorVentaProducto (ventas, x:xs, mayor, cantMayor) =
    let 
        cantVentas = cantVentasProducto (x, ventas)
    in
        if cantVentas > cantMayor
            then mayorVentaProducto (ventas, xs, x, cantVentas)
            else mayorVentaProducto (ventas, xs, mayor, cantMayor)

--función que guarda en un archivo la información del producto con mayor cantidad de ventas
--recibe el nombre del producto y la cantidad, pregunta al usuario el nombre del archivo
--y guarda la información en formato CSV con codificación UTF-8
guardarMayorVentaProducto :: (String, Int) -> IO()
guardarMayorVentaProducto(nombre, cantidad) = 
    let contenido = "Nombre,Cantidad\n" ++ nombre ++ "," ++ show cantidad
    in do
        archivo <- preguntaGuardar  
        withFile archivo WriteMode $ \handle -> do
            hSetEncoding handle utf8
            hPutStr handle contenido
        putStrLn "Se ha guardado la estadistica correctamente"

--función principal que imprime el producto más vendido
--obtiene todos los productos, identifica el producto con mayor cantidad de ventas y su nombre con funciones externas
--imprime la información en pantalla y llama a la función de guardado
imprimirMayorVentaProducto :: [Venta] -> IO()
imprimirMayorVentaProducto ventas = 
    let 
        productos = todosLosProductos ventas
        mayorProducto = mayorVentaProducto (ventas, productos, 0,0)
        cantidadProducto = cantVentasProducto (mayorProducto, ventas)
        nombreMayor = nombreProducto (mayorProducto, ventas)
    in do
        putStrLn "-------- Producto más vendido --------"
        if cantidadProducto == 1 then putStrLn ( "Producto: " ++ nombreMayor ++ " | Cantidad: " ++ show cantidadProducto ++ " venta")
        else putStrLn ("Producto: " ++ nombreMayor ++ " | Cantidad: " ++ show cantidadProducto ++ " ventas")
        guardarMayorVentaProducto (nombreMayor, cantidadProducto)

--función auxiliar que cuenta la cantidad de ventas de un producto específico
--recibe el ProductoId, la lista de ventas y un acumulador inicial
--recorre recursivamente la lista de ventas y suma la cantidad de ese producto
cantVentasProductoAux :: (ProductoId, [Venta], Int) -> Int
cantVentasProductoAux (_, [], res) = res
cantVentasProductoAux (prod, x:xs, res) =
  if producto_id x == prod
     then cantVentasProductoAux (prod, xs, res + cantidad x)
     else cantVentasProductoAux (prod, xs, res)

--función principal que devuelve la cantidad total de ventas de un producto
--recibe un ProductoId y la lista de ventas, inicializa el acumulador en 0
cantVentasProducto :: (ProductoId , [Venta]) -> Int
cantVentasProducto (id, ventas) = cantVentasProductoAux (id, ventas, 0)

--función auxiliar que cuenta la cantidad de ventas de una categoría específica
--recibe la categoría, la lista de ventas y un acumulador inicial
--recorre recursivamente la lista de ventas y suma la cantidad de esa categoría
cantVentasCategoriaAux :: (Categoria, [Venta], Int) -> Int
cantVentasCategoriaAux (_, [], res) = res
cantVentasCategoriaAux (cat, x:xs, res) =
  if categoria x == cat
     then cantVentasCategoriaAux (cat, xs, res + cantidad x)
     else cantVentasCategoriaAux (cat, xs, res)

--función principal que devuelve la cantidad total de ventas de una categoría
--recibe la categoría y la lista de ventas, inicializa el acumulador en 0
cantVentasCategoria :: (Categoria , [Venta]) -> Int
cantVentasCategoria (cat, ventas) = cantVentasCategoriaAux (cat, ventas, 0)

--función que encuentra la categoría con menor cantidad de ventas
--recibe la lista de ventas, la lista de categorías, la categoría menor actual y su cantidad
--recorre recursivamente la lista de categorías y devuelve la categoría con menor participación
menorCantVentaCategoria :: ([Venta], [Categoria], Categoria, Int) -> Categoria
menorCantVentaCategoria (_, [], menor, _) = menor
menorCantVentaCategoria (ventas, x:xs, menor, cantMenor) =
    let 
        cantVentas = cantVentasCategoria (x, ventas)
    in
        if cantVentas < cantMenor
            then menorCantVentaCategoria (ventas, xs, x, cantVentas)
            else menorCantVentaCategoria (ventas, xs, menor, cantMenor)

--función que devuelve el nombre de una categoría específica
--recibe la categoría y la lista de ventas, si no la encuentra devuelve "Categoría no encontrada"
nombreCategoria :: (Categoria, [Venta]) -> String
nombreCategoria (_, []) = "Categoría no encontrada"
nombreCategoria (cat, x:xs) =
    if categoria x == cat
        then categoria x
        else nombreCategoria (cat, xs)

--función que guarda en un archivo la categoría con menor cantidad de ventas
--recibe el nombre de la categoría y la cantidad, pregunta al usuario el archivo donde guardar
--y lo guarda en formato CSV con codificación UTF-8
guardarMenorCantVentaCategoria :: (String, Int) -> IO()
guardarMenorCantVentaCategoria (nombre, cantidad) = 
    let contenido = "Nombre,Cantidad\n" ++ nombre ++ "," ++ show cantidad
    in do
        archivo <- preguntaGuardar  
        withFile archivo WriteMode $ \handle -> do
            hSetEncoding handle utf8
            hPutStr handle contenido
        putStrLn "Se ha guardado la estadistica correctamente"

--función principal que imprime la categoría con menor participación
--obtiene todas las categorías, identifica la categoría con menor cantidad de ventas y su nombre
--imprime la información en pantalla y llama a la función de guardado
imprimirMenorCantVentaCategoria :: [Venta] -> IO()
imprimirMenorCantVentaCategoria ventas = 
    let 
        categorias = todasLasCategorias ventas
        menorCategoria = menorCantVentaCategoria (ventas, categorias, "",999999)
        cantidadCategoria = cantVentasCategoria (menorCategoria, ventas)
        nombreMenor = nombreCategoria (menorCategoria, ventas)
    in do
        putStrLn "-------- Categoría con menor participación --------"
        if cantidadCategoria == 1 then putStrLn ("Categoría: " ++ nombreMenor ++ " | Cantidad: " ++ show cantidadCategoria ++ " participación")
        else putStrLn ("Categoría: " ++ nombreMenor ++ "| Cantidad: " ++ show cantidadCategoria ++ " participaciones")
        guardarMenorCantVentaCategoria (nombreMenor, cantidadCategoria)


--función auxiliar que imprime la cantidad de ventas por categoría
--recibe la lista de categorías, la lista de ventas y recorre recursivamente las categorías
--imprime cada categoría con su cantidad de ventas
imprimirCantVentasCategoriasAux  :: ([Categoria], [Venta]) -> IO ()
imprimirCantVentasCategoriasAux ([], _) = return ()
imprimirCantVentasCategoriasAux (cat:xs, ventas) = do
    putStrLn ("Categoría: " ++ cat ++ " | Cantidad: " ++ show (cantVentasCategoria (cat, ventas)))
    putStrLn "---------------------------"
    imprimirCantVentasCategoriasAux (xs, ventas)

--función que parsea la cantidad de ventas por categoría para guardarlo en un archivo CSV
--recibe la lista de categorías, la lista de ventas y un string con el contenido acumulado
--devuelve un string con todas las categorías y su cantidad de ventas formateadas
parsearCantVentasCategorias :: ([Categoria], [Venta], String) -> String
parsearCantVentasCategorias ([], _, contenido) = contenido
parsearCantVentasCategorias (x:xs, ventas, contenido) =
    let
        cantidad = cantVentasCategoria (x, ventas)
        linea = "1," ++ x ++ "," ++ show cantidad ++ "\n"
        nuevoCont = contenido ++ linea
    in parsearCantVentasCategorias (xs, ventas, nuevoCont)

--función principal que imprime la cantidad de ventas por categoría
--obtiene todas las categorías presentes en las ventas
--llama a la función auxiliar para imprimir cada categoría con su cantidad de ventas
imprimirCantVentasCategorias :: [Venta] -> IO()
imprimirCantVentasCategorias ventas =
    let
        categorias = todasLasCategorias ventas
    in do
        putStrLn "-------- Reporte 1: Cantidad de Ventas por Categoría --------\n"
        imprimirCantVentasCategoriasAux (categorias, ventas)

--función auxiliar que busca la venta con mayor total (la más alta)
--recibe la lista de ventas, la venta actual mayor y el total mayor acumulado
--recorre recursivamente la lista de ventas y devuelve la venta con mayor total
ventaMasAltaAux :: ([Venta], Venta, Float) -> Venta
ventaMasAltaAux ([], mayor, _) = mayor
ventaMasAltaAux (x:xs, mayor, cantMayor) =
    if total x > cantMayor 
        then ventaMasAltaAux (xs, x, total x) 
        else ventaMasAltaAux (xs, mayor, cantMayor)

--función principal que devuelve la venta mas alta
--recibe la lista de ventas y llama a la función auxiliar inicializando con la primera venta
ventaMasAlta :: [Venta] -> Venta
ventaMasAlta (x:xs) = ventaMasAltaAux (xs, x, total x)

--función auxiliar que busca la venta con menor total (mas baja)
--recibe la lista de ventas, la venta actual menor y el total menor acumulado
--recorre recursivamente la lista de ventas y devuelve la venta con menor total
ventaMasBajaAux :: ([Venta], Venta, Float) -> Venta
ventaMasBajaAux ([], menor, _) = menor
ventaMasBajaAux (x:xs, menor, cantMenor) =
    if total x < cantMenor
        then ventaMasBajaAux (xs, x, total x)
        else ventaMasBajaAux (xs, menor, cantMenor)

--función principal que devuelve la venta mas baja
--recibe la lista de ventas y llama a la función auxiliar inicializando con la primera venta
ventaMasBaja :: [Venta] -> Venta
ventaMasBaja (x:xs) = ventaMasBajaAux (xs, x, total x)

--función que imprime la información de una venta
--recibe una venta y muestra su id, nombre de producto y total 
imprimirVenta :: Venta -> IO()
imprimirVenta venta = do 
        putStrLn ("Venta: " ++ show (venta_id venta) ++ " | Producto: " ++ producto_nombre venta ++ " | Total: " ++ show (total venta))

--función que pregunta al usuario el nombre del archivo donde desea guardar una estadística
--si el usuario ingresa un nombre vacío, se vuelve a preguntar
--si el nombre no termina en .csv, se agrega automáticamente la extensión
preguntaGuardar :: IO String
preguntaGuardar = do
    putStrLn "\nIngrese el nombre del archivo (CSV) donde desea guardar la estadística:"
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

--función que parsea las ventas más alta y más baja para guardarlas en un CSV
--recibe la lista de ventas y genera un string con la venta más alta y la más baja
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

--función que imprime las ventas más alta y más baja
--obtiene las ventas con mayor y menor total, e imprime la información en pantalla
imprimirVentaAltaBaja :: [Venta] -> IO()
imprimirVentaAltaBaja ventas = 
    let
        ventaAlta = ventaMasAlta ventas
        ventaBaja = ventaMasBaja ventas
    in do
        putStrLn "\n-------- Reporte 2: Venta más alta --------\n"
        imprimirVenta ventaAlta
        putStrLn "\n-------- Reporte 3: Venta más baja --------\n"
        imprimirVenta ventaBaja


--función auxiliar que devuelve todos los productos de una categoría específica
--recibe la lista de ventas, la categoría a filtrar y una lista acumuladora de productos
--recorre recursivamente las ventas y agrega los productos de la categoría sin repetirlos
productosPorCategoriaAux :: ([Venta], Categoria, [ProductoId]) -> [ProductoId]
productosPorCategoriaAux ([], _, productos) = productos
productosPorCategoriaAux (x:xs, cat, productos) =
    if categoria x == cat
        then 
            if producto_id x `elem` productos
                then productosPorCategoriaAux (xs, cat, productos)
                else productosPorCategoriaAux (xs, cat, producto_id x : productos)
        else productosPorCategoriaAux (xs, cat, productos)

--función principal que devuelve todos los productos de una categoría específica
--recibe la lista de ventas y la categoría, llama a la función auxiliar inicializando la lista de productos vacía
productosPorCategoria :: ([Venta], Categoria) -> [ProductoId]
productosPorCategoria (ventas, cat) = productosPorCategoriaAux (ventas, cat, [])

--función que devuelve la cantidad de productos distintos de una categoría
--recibe la lista de ventas y la categoría, llama a productosPorCategoria y devuelve el largo
cantProductosCategoria :: ([Venta], Categoria) -> Int
cantProductosCategoria (ventas, cat) = length (productosPorCategoria(ventas, cat))

--función auxiliar que determina la categoría con mayor variedad de productos
--recibe la lista de ventas, la lista de categorías, la categoría actual con mayor cantidad de productos y la cantidad máxima
--recorre recursivamente las categorías y devuelve la que tenga mayor cantidad de productos diferentes
categoriaMasVariadaAux :: ([Venta], [Categoria], Categoria, Int) -> Categoria
categoriaMasVariadaAux (_, [], mayor, _) = mayor
categoriaMasVariadaAux (ventas, x:xs, mayor, cantMayor) =
    let
        cantProductos = cantProductosCategoria (ventas, x)
    in
        if cantProductos > cantMayor
            then categoriaMasVariadaAux (ventas, xs, x, cantProductos)
            else categoriaMasVariadaAux (ventas, xs, mayor, cantMayor)

--función principal que devuelve la categoría con mayor variedad de productos
--obtiene todas las categorías presentes en las ventas
--inicializa con la primera categoría y su cantidad de productos
--llama a la función auxiliar para determinar la categoría más variada
categoriaMasVariada :: [Venta] -> Categoria
categoriaMasVariada ventas =
    let 
        categorias = todasLasCategorias ventas
        primera = head categorias
        cantInicial = cantProductosCategoria (ventas, primera)
    in
        categoriaMasVariadaAux (ventas, categorias, primera, cantInicial)

--función que parsea la categoría más variada para guardarla en un CSV
--recibe la lista de ventas y genera un string con la categoría y la cantidad de productos
parsearCategoriaMasVariada :: [Venta] -> String
parsearCategoriaMasVariada ventas =
    let 
        categoria = categoriaMasVariada ventas
        cantidad = cantProductosCategoria (ventas, categoria)
        contenido = "Reporte,Categoria,Cantidad\n4," ++ categoria ++ "," ++ show cantidad
    in
        contenido

--función auxiliar que imprime la información de la categoría más variada
--recibe la lista de ventas y la categoría, imprime el nombre de la categoría y la cantidad de productos
imprimirCategoriaMasVariadaAux :: ([Venta], Categoria) -> IO()
imprimirCategoriaMasVariadaAux (ventas, categoria) = 
    let 
        cantProductos = cantProductosCategoria (ventas, categoria)
    in
        if cantProductos == 1 then putStrLn ("Categoría: " ++ categoria ++ " | Cantidad: " ++ show cantProductos ++ " producto")
        else putStrLn ("Categoría: " ++ categoria ++ " | Cantidad: " ++ show cantProductos ++ " productos")

--función principal que imprime la categoría con mayor variedad de productos
--obtiene la categoría más variada y llama a la función auxiliar para imprimir su información
imprimirCategoriaMasVariada :: [Venta] -> IO()
imprimirCategoriaMasVariada ventas = 
    let 
        categoria = categoriaMasVariada ventas 
    in do
        putStrLn "\n-------- Reporte 4: Categoría más variada --------\n"
        imprimirCategoriaMasVariadaAux (ventas, categoria)

--función que guarda un resumen general de estadísticas en un archivo CSV
--recibe la lista de ventas, genera los diferentes reportes en formato string
--concatena el reporte de cantidad de ventas por categoría, ventas altas y bajas, y categoría más variada
--pregunta al usuario el nombre del archivo y lo guarda con codificación UTF-8
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

--función que imprime un resumen general de estadísticas
--recibe la lista de ventas, imprime la cantidad de ventas por categoría, la venta más alta y baja, y la categoría más variada
--al final llama a la función que guarda este resumen en un archivo
imprimirResumenGeneral :: [Venta] -> IO()
imprimirResumenGeneral ventas = do 
    putStrLn "---------- Resumen General ----------\n"
    imprimirCantVentasCategorias ventas
    imprimirVentaAltaBaja ventas
    imprimirCategoriaMasVariada ventas
    guardarResumenGeneral ventas

--función que guarda un texto en un archivo
--recibe el nombre del archivo y el texto a guardar, lo sobrescribe si ya existe
guardarEnArchivo :: (String, String) -> IO()
guardarEnArchivo (archivo, texto) = do
    writeFile archivo texto
    putStrLn "Archivo creado o sobrescrito con éxito."

--menú interactivo de estadísticas
--recibe las ventas, verifica si hay datos cargados
--permite al usuario seleccionar las estadisticas disponibles
--llama a las funciones correspondientes según la opción elegida 
--y vuelve a mostrar el menú hasta que el usuario decida regresar al menu principal
menuEstadisticas :: Ventas -> IO ()
menuEstadisticas (Ventas ventas) = do
    if null ventas
       then putStrLn "\nNo hay ventas cargadas. Importe datos primero."
       else do
           putStrLn "\n-------- Estadísticas --------"
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


