{-# LANGUAGE DeriveGeneric #-}

module Logicadenegocios.Estructuras where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

type VentaId = Int 
type Fecha = String
type ProductoId = Int
type NombreProducto = String
type Categoria = String
type Cantidad = Int
type PrecioUnitario = Float
type Total = Float

data Venta = Venta
  { venta_id        :: VentaId
  , fecha           :: Fecha
  , producto_id     :: ProductoId
  , producto_nombre :: NombreProducto 
  , categoria       :: Categoria
  , cantidad        :: Cantidad
  , precio_unitario :: PrecioUnitario
  , total           :: Total
  } deriving (Show, Generic)

instance FromJSON Venta

data Ventas = Ventas [Venta] deriving Show
