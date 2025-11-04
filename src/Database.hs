{-# LANGUAGE OverloadedStrings #-}

module Database
  ( initDatabase
  , loadVehiculos
  , loadUsuarios
  , saveVehiculos
  , saveUsuarios
  ) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.ToRow
import Data.Time (Day)
import Models
import Users

------------------------------------------------------------
-- 🧱 Inicializa la base de datos y crea tablas si no existen
------------------------------------------------------------
initDatabase :: IO Connection
initDatabase = do
  conn <- open "garage.db"

  execute_ conn "CREATE TABLE IF NOT EXISTS usuarios (\
                \id INTEGER PRIMARY KEY AUTOINCREMENT,\
                \nombre TEXT,\
                \email TEXT UNIQUE,\
                \password TEXT,\
                \rol TEXT)"
  execute_ conn "CREATE TABLE IF NOT EXISTS vehiculos (\
                \id INTEGER PRIMARY KEY AUTOINCREMENT,\
                \ownerId INTEGER,\
                \tipo TEXT,\
                \marca TEXT,\
                \modelo TEXT,\
                \anio INTEGER,\
                \color TEXT,\
                \kilometros INTEGER,\
                \ultimaRevision TEXT,\
                \itvFecha TEXT,\
                \foto TEXT,\
                \notas TEXT)"

  -- Crear admin por defecto si no existe
  rows <- query_ conn "SELECT id FROM usuarios WHERE email='admin@example.com'" :: IO [Only Int]
  case rows of
    [] -> do
      putStrLn "Creando usuario admin por defecto..."
      execute conn
        "INSERT INTO usuarios (nombre,email,password,rol) VALUES (?,?,?,?)"
        ("Admin" :: String, "admin@example.com" :: String, "admin" :: String, "Admin" :: String)
    _ -> pure ()

  return conn

------------------------------------------------------------
-- 🧩 Instancias personalizadas para Vehiculo
------------------------------------------------------------
instance FromRow Vehiculo where
  fromRow = do
    i   <- field
    oid <- field
    t   <- field
    ma  <- field
    mo  <- field
    a   <- field
    c   <- field
    km  <- field
    ur  <- field :: RowParser (Maybe String)
    itv <- field :: RowParser (Maybe String)
    f   <- field
    n   <- field
    return $
      Vehiculo i oid t ma mo a c km
        (parseDay ur)
        (parseMaybeDay itv)
        f n
    where
      parseDay (Just s) = read s
      parseDay Nothing  = toEnum 0
      parseMaybeDay (Just s) = Just (read s)
      parseMaybeDay Nothing  = Nothing

instance ToRow Vehiculo where
  toRow v =
    [ toField (vehiculoId v)
    , toField (vehiculoOwnerId v)
    , toField (tipo v)
    , toField (marca v)
    , toField (modelo v)
    , toField (anio v)
    , toField (color v)
    , toField (kilometros v)
    , toField (show (ultimaRevision v))
    , toField (fmap show (itvFecha v))
    , toField (foto v)
    , toField (notas v)
    ]

------------------------------------------------------------
-- 🧩 Cargar todos los usuarios desde SQLite
------------------------------------------------------------
loadUsuarios :: Connection -> IO [Usuario]
loadUsuarios conn = do
  rows <- query_ conn
    "SELECT id,nombre,email,password,rol FROM usuarios"
    :: IO [(Int,String,String,String,String)]
  return [Usuario i n e p (if r == "Admin" then Admin else User) | (i,n,e,p,r) <- rows]

------------------------------------------------------------
-- 🧩 Cargar todos los vehículos desde SQLite
------------------------------------------------------------
loadVehiculos :: Connection -> IO [Vehiculo]
loadVehiculos conn = query_ conn
  "SELECT id,ownerId,tipo,marca,modelo,anio,color,kilometros,ultimaRevision,itvFecha,foto,notas FROM vehiculos"

------------------------------------------------------------
-- 🧩 Guardar lista de usuarios en la base
------------------------------------------------------------
saveUsuarios :: Connection -> [Usuario] -> IO ()
saveUsuarios conn usuarios = do
  execute_ conn "DELETE FROM usuarios"
  mapM_ (\u -> execute conn
    "INSERT INTO usuarios (id,nombre,email,password,rol) VALUES (?,?,?,?,?)"
    (userId u, nombre u, email u, password u, show (rol u))) usuarios

------------------------------------------------------------
-- 🧩 Guardar lista de vehículos en la base
------------------------------------------------------------
saveVehiculos :: Connection -> [Vehiculo] -> IO ()
saveVehiculos conn vehiculos = do
  execute_ conn "DELETE FROM vehiculos"
  mapM_ (\v -> execute conn
    "INSERT INTO vehiculos (id,ownerId,tipo,marca,modelo,anio,color,kilometros,ultimaRevision,itvFecha,foto,notas)\
    \VALUES (?,?,?,?,?,?,?,?,?,?,?,?)" v) vehiculos