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
