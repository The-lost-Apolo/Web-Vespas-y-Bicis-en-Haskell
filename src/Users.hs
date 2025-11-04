{-# LANGUAGE DeriveGeneric #-}

module Users where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Rol = Admin | User deriving (Show, Eq, Generic)

instance ToJSON Rol
instance FromJSON Rol

data Usuario = Usuario
  { userId   :: Int
  , nombre   :: String
  , email    :: String
  , password :: String
  , rol      :: Rol
  } deriving (Show, Eq, Generic)

instance ToJSON Usuario
instance FromJSON Usuario
