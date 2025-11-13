{-# LANGUAGE OverloadedStrings #-}

module Database
  ( initDatabase
  , loadVehiculos
  , loadUsuarios
  , saveVehiculos
  , saveUsuarios
  , loadRutas
  , saveRutaGetId
  , loadParadas
  , saveParada
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

  -- 👤 Tabla de usuarios
  execute_ conn "CREATE TABLE IF NOT EXISTS usuarios (\
                \id INTEGER PRIMARY KEY AUTOINCREMENT,\
                \nombre TEXT,\
                \email TEXT UNIQUE,\
                \password TEXT,\
                \rol TEXT)"

  -- 🚗 Tabla de vehículos
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

  -- 🗺️ Tabla de rutas
  execute_ conn "CREATE TABLE IF NOT EXISTS rutas (\
                \id INTEGER PRIMARY KEY AUTOINCREMENT,\
                \userId INTEGER,\
                \fecha TEXT,\
                \distancia REAL,\
                \duracion REAL,\
                \velocidadMedia REAL)"

  -- 📍 Tabla de paradas
  execute_ conn "CREATE TABLE IF NOT EXISTS paradas (\
                \id INTEGER PRIMARY KEY AUTOINCREMENT,\
                \rutaId INTEGER,\
                \latitud REAL,\
                \longitud REAL,\
                \descripcion TEXT,\
                \foto TEXT,\
                \video TEXT)"

  putStrLn "📦 Tablas listas."

  -- Crear admin por defecto
  rows <- query_ conn "SELECT id FROM usuarios WHERE email='admin@example.com'" :: IO [Only Int]
  case rows of
    [] -> execute conn
          "INSERT INTO usuarios (nombre,email,password,rol) VALUES (?,?,?,?)"
          ("Admin" :: String, "admin@example.com" :: String, "admin" :: String, "Admin" :: String)
    _  -> pure ()

  return conn

------------------------------------------------------------
-- 🧩 Instancias personalizadas
------------------------------------------------------------
instance FromRow Ruta where
  fromRow = Ruta <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Ruta where
  toRow r =
    [ toField (rutaUserId r)
    , toField (rutaFecha r)
    , toField (rutaDistancia r)
    , toField (rutaDuracion r)
    , toField (rutaVelMedia r)
    ]

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
-- 👥 Usuarios
------------------------------------------------------------
loadUsuarios :: Connection -> IO [Usuario]
loadUsuarios conn = do
  rows <- query_ conn
    "SELECT id,nombre,email,password,rol FROM usuarios"
      :: IO [(Int,String,String,String,String)]
  return [Usuario i n e p (if r == "Admin" then Admin else User)
         | (i,n,e,p,r) <- rows]

saveUsuarios :: Connection -> [Usuario] -> IO ()
saveUsuarios conn usuarios = do
  execute_ conn "DELETE FROM usuarios"
  mapM_ (\u -> execute conn
      "INSERT INTO usuarios (id,nombre,email,password,rol) VALUES (?,?,?,?,?)"
      (userId u, nombre u, email u, password u, show (rol u))
    ) usuarios

------------------------------------------------------------
-- 🚗 Vehículos
------------------------------------------------------------
loadVehiculos :: Connection -> IO [Vehiculo]
loadVehiculos conn = query_ conn
  "SELECT id,ownerId,tipo,marca,modelo,anio,color,kilometros,ultimaRevision,itvFecha,foto,notas FROM vehiculos"

saveVehiculos :: Connection -> [Vehiculo] -> IO ()
saveVehiculos conn vehiculos = do
  execute_ conn "DELETE FROM vehiculos"
  mapM_ (\v -> execute conn
      "INSERT INTO vehiculos (id,ownerId,tipo,marca,modelo,anio,color,kilometros,ultimaRevision,itvFecha,foto,notas)\
      \VALUES (?,?,?,?,?,?,?,?,?,?,?,?)" v
    ) vehiculos

------------------------------------------------------------
-- 🗺️ Rutas
------------------------------------------------------------

loadRutas :: Connection -> IO [Ruta]
loadRutas conn = query_ conn
  "SELECT id,userId,fecha,distancia,duracion,velocidadMedia FROM rutas ORDER BY id DESC"

-- ⭐ NUEVA FUNCIÓN: insertar ruta y devolver ID generado
saveRutaGetId :: Connection -> Ruta -> IO Int
saveRutaGetId conn r = do
  execute conn
    "INSERT INTO rutas (userId,fecha,distancia,duracion,velocidadMedia) VALUES (?,?,?,?,?)"
    (rutaUserId r, rutaFecha r, rutaDistancia r, rutaDuracion r, rutaVelMedia r)
  rid <- lastInsertRowId conn
  return (fromIntegral rid)

------------------------------------------------------------
-- 📍 Paradas
------------------------------------------------------------
instance FromRow Parada where
  fromRow = Parada <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Parada where
  toRow p =
    [ toField (paradaRutaId p)
    , toField (paradaLat p)
    , toField (paradaLong p)
    , toField (paradaDesc p)
    , toField (paradaFoto p)
    , toField (paradaVideo p)
    ]

loadParadas :: Connection -> Int -> IO [Parada]
loadParadas conn rutaId = query conn
  "SELECT id,rutaId,latitud,longitud,descripcion,foto,video FROM paradas WHERE rutaId = ?"
  (Only rutaId)

saveParada :: Connection -> Parada -> IO Int
saveParada conn p = do
  execute conn
    "INSERT INTO paradas (rutaId,latitud,longitud,descripcion,foto,video) VALUES (?,?,?,?,?,?)"
    (paradaRutaId p, paradaLat p, paradaLong p, paradaDesc p, paradaFoto p, paradaVideo p)
  r <- lastInsertRowId conn
  return (fromIntegral r)