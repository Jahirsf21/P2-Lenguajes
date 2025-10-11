module Logicadenegocios.ImportacionDatos (menuImportarDatos) where
import Logicadenegocios.Estructuras


import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson (decode)


-- Función para validar los campos de los identificadores de las ventas y del producto relacionado a la venta
-- Deben ser distintos de 0 para ser considerados válidos
validarIdentificadores :: Venta -> Bool
validarIdentificadores v = venta_id v /= 0 && producto_id v /= 0

-- Función para validar los campos de texto del nombre de producto y nombre de categoria
-- Debe ser distinto de null o vacío "" para ser considerados válidos
validarTexto :: Venta -> Bool
validarTexto v = not (null $ producto_nombre v) && not (null $ categoria v)

-- Función para validar el campo de la fecha de la venta
-- Debe ser distinto de null o vacío "" para ser considerada válida
validarFecha :: Venta -> Bool
validarFecha v = not (null $ fecha v)

-- Función para validar verificar que la venta tenga datos validos
-- Debe cumplirse las 3 validaciones anteriores
esVentaValida :: Venta -> Either [String] ()
esVentaValida v =
  let errores = concat
        [ if not (validarIdentificadores v) then ["Identificadores inválidos (venta_id o producto_id)"] else []
        , if not (validarFecha v) then ["Fecha vacía"] else []
        , if not (validarTexto v) then ["Nombre o categoría de producto vacíos"] else []
        ]
  in if null errores then Right () else Left errores

-- Función para separar las ventas válidas de las inválidas
-- Regresa una tupla con las ventas válidas e inválidas y sus respectivos errores
separarVentas :: [Venta] -> ([Venta], [(Venta, [String])])
separarVentas = foldr clasificar ([], [])
  where
    clasificar venta (validas, invalidas) =
      case esVentaValida venta of
        Right ()     -> (venta : validas, invalidas)
        Left errores -> (validas, (venta, errores) : invalidas)

-- Función para imprimir la lista de errores de las ventas inválidas
imprimirErroresVenta :: (Venta, [String]) -> IO ()
imprimirErroresVenta (venta, errores) = do
  putStrLn $ "\nVenta excluida: " ++ show venta
  putStrLn "Motivos:"
  mapM_ (\e -> putStrLn ("  - " ++ e)) errores

-- Función para mostrar el menú de la importación de datos
-- Solicita la ruta del archivo de ventas .json
menuImportarDatos :: IO Ventas
menuImportarDatos = do
  putStrLn "Ingrese el nombre del archivo JSON de ventas:"
  archivo <- getLine
  contenido <- B.readFile archivo
  let ventasDecodificadas = decode contenido :: Maybe [Venta]

  case ventasDecodificadas of
    Just nuevasVentas -> do
      let (ventasValidas, ventasInvalidas) = separarVentas nuevasVentas
      let ventasActualizadas = Ventas ventasValidas

      putStrLn $ "\nVentas válidas importadas: " ++ show (length ventasValidas) ++ " registros"

      if null ventasInvalidas
        then putStrLn "\nTodas las líneas fueron válidas."
        else do
          putStrLn "\nLas siguientes líneas fueron excluidas por errores:"
          mapM_ imprimirErroresVenta ventasInvalidas

      return ventasActualizadas

    Nothing -> do
      putStrLn "Error al parsear el archivo de ventas."
      return (Ventas [])