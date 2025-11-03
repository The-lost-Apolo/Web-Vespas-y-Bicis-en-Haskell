{-# LANGUAGE DeriveGeneric #-}

module Models where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time (Day, fromGregorian)

data Vehiculo = Vehiculo
  { vehiculoId       :: Int
  , tipo             :: String
  , marca            :: String
  , modelo           :: String
  , anio             :: Int
  , color            :: String
  , kilometros       :: Int
  , ultimaRevision   :: Day
  , itvFecha         :: Maybe Day
  , foto             :: Maybe FilePath
  , notas            :: String
  } deriving (Show, Eq, Generic)

instance ToJSON Vehiculo
instance FromJSON Vehiculo

-- Genera el siguiente ID
nextId :: [Vehiculo] -> Int
nextId [] = 1
nextId vs = maximum (map vehiculoId vs) + 1