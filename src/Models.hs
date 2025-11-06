{-# LANGUAGE DeriveGeneric #-}

module Models where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time (Day, fromGregorian)

------------------------------------------------------------
-- 🚗 Vehículos
------------------------------------------------------------
data Vehiculo = Vehiculo
  { vehiculoId :: Int
  , vehiculoOwnerId :: Int
  , tipo :: String
  , marca :: String
  , modelo :: String
  , anio :: Int
  , color :: String
  , kilometros :: Int
  , ultimaRevision :: Day
  , itvFecha :: Maybe Day
  , foto :: Maybe String
  , notas :: String
  } deriving (Show, Eq, Generic)

instance ToJSON Vehiculo
instance FromJSON Vehiculo

-- Genera el siguiente ID
nextId :: [Vehiculo] -> Int
nextId [] = 1
nextId vs = maximum (map vehiculoId vs) + 1

--------------------------------------------------------
-- 🗺️ Modelos de rutas y paradas
--------------------------------------------------------
data Ruta = Ruta
  { rutaId        :: Int
  , rutaUserId    :: Int
  , rutaFecha     :: String
  , rutaDistancia :: Double
  , rutaDuracion  :: Double
  , rutaVelMedia  :: Double
  } deriving (Show, Eq, Generic)

instance ToJSON Ruta
instance FromJSON Ruta

data Parada = Parada
  { paradaId     :: Int
  , paradaRutaId :: Int
  , paradaLat    :: Double
  , paradaLong   :: Double
  , paradaDesc   :: String
  , paradaFoto   :: Maybe String
  , paradaVideo  :: Maybe String
  } deriving (Show, Eq, Generic)

instance ToJSON Parada
instance FromJSON Parada
