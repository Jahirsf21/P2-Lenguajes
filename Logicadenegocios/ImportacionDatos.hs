module Logicadenegocios.ImportacionDatos (menuImportarDatos) where
import Logicadenegocios.Estructuras


import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson (decode)

menuImportarDatos :: IO ()
menuImportarDatos = do
  putStrLn "Ingrese el nombre del archivo JSON de ventas:"
  archivo <- getLine
  contenido <- B.readFile archivo
  let ventasDecodificadas = decode contenido :: Maybe [Venta]
  let ventasIniciales = Ventas []

  case ventasDecodificadas of
    Just nuevasVentas -> do
      let ventasActualizadas = Ventas nuevasVentas
      putStrLn $ "Ventas importadas: " ++ show ventasActualizadas
    Nothing -> putStrLn "Error al parsear el archivo de ventas."
    
agregarVenta :: Venta -> Ventas -> Ventas
agregarVenta v (Ventas vs) = Ventas (v : vs)
