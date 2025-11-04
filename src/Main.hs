{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status302)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import Data.List (find)
import Data.Time (fromGregorian)
import Lucid (renderBS)
import Text.Read (readMaybe)
import Data.Char (isDigit)
import Control.Concurrent.MVar
import Control.Monad (void)
import Database.SQLite.Simple (Connection)

import Pages
import Models
import Users
import Database

--------------------------------------------------------
-- 🔧 Helpers
--------------------------------------------------------

takeIdAfter :: String -> BS.ByteString -> Maybe Int
takeIdAfter prefix p =
  let rest = drop (length prefix) (BS.unpack p)
      digits = takeWhile isDigit rest
  in if null digits then Nothing else readMaybe digits

readDef :: Read a => a -> String -> a
readDef def s = case reads s of
  [(x, "")] -> x
  _         -> def

--------------------------------------------------------
-- 🚀 Main
--------------------------------------------------------

main :: IO ()
main = do
  conn <- initDatabase
  usuarioActual <- newMVar Nothing
  putStrLn "✅ Servidor iniciado en http://localhost:8080"
  run 8080 (app conn usuarioActual)

--------------------------------------------------------
-- 🌐 Aplicación principal
--------------------------------------------------------

app :: Connection -> MVar (Maybe Usuario) -> Application
app conn usuarioActual req respond = do
  usuarios  <- loadUsuarios conn
  vehiculos <- loadVehiculos conn
  currentUser <- readMVar usuarioActual
  let path = rawPathInfo req

  case (requestMethod req, path) of

    -----------------------------------------
    -- 🏠 INICIO
    -----------------------------------------
    ("GET", "/") -> do
      let header = case currentUser of
            Just u ->
              "<p>Bienvenido, <b>" <> nombre u <> "</b> 👋 — " <>
              (if rol u == Admin then "<a href='/admin' style='color:gold'>🛠️ Panel de administración</a> | " else "") <>
              "<a href='/logout' style='color:lightgreen'>Cerrar sesión</a></p>"
            Nothing ->
              "<p><a href='/login'>Iniciar sesión</a> | <a href='/registro'>Crear cuenta</a></p>"
      respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] $
        BL.pack $ "<!DOCTYPE html><html><head><meta charset='UTF-8'><title>Inicio</title>\
        \<style>body{background:#111;color:white;font-family:sans-serif;}a{color:lightgreen;}</style>\
        \</head><body><h1>🚗 Bienvenido a Mi Garaje Vespa</h1>" <>
        header <>
        "<a href='/garaje'>Entrar al garaje</a></body></html>"

    -----------------------------------------
    -- 🔐 LOGIN
    -----------------------------------------
    ("GET", "/login") ->
      respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] $
        "<html><head><meta charset='UTF-8'><title>Login</title></head><body>\
        \<h1>Iniciar sesión</h1>\
        \<form method='post' action='/login'>\
        \Email: <input name='email'><br>\
        \Contraseña: <input type='password' name='password'><br>\
        \<input type='submit' value='Entrar'>\
        \</form>\
        \<p><a href='/'>Volver al inicio</a></p>\
        \</body></html>"

    ("POST", "/login") -> do
      (params, _) <- parseRequestBody lbsBackEnd req
      let lookupField k = maybe "" BS.unpack (lookup k params)
          email = lookupField "email"
          pwd   = lookupField "password"
      case find (\u -> email == Users.email u && pwd == password u) usuarios of
        Just u -> do
          swapMVar usuarioActual (Just u)
          respond $ responseLBS status302 [("Location", "/garaje")] ""
        Nothing -> respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] $
          "<!DOCTYPE html><html><head><meta charset='UTF-8'><title>Error de login</title>\
          \<style>body{background:#111;color:white;font-family:sans-serif;text-align:center;}a{color:lightgreen;text-decoration:none;margin:10px;display:inline-block;}</style>\
          \</head><body><h1>⚠️ Usuario o contraseña incorrectos</h1>\
          \<p>El usuario no existe o la contraseña es incorrecta.</p>\
          \<p><a href='/registro'>📝 Crear cuenta</a> <a href='/'>🏠 Volver al inicio</a></p>\
          \</body></html>"

    -----------------------------------------
    -- 🔒 LOGOUT
    -----------------------------------------
    ("GET", "/logout") -> do
      swapMVar usuarioActual Nothing
      respond $ responseLBS status302 [("Location", "/")] ""

    -----------------------------------------
    -- 🧾 REGISTRO
    -----------------------------------------
    ("GET", "/registro") ->
      respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] $
        "<html><head><meta charset='UTF-8'><title>Registro</title></head><body>\
        \<h1>Crear usuario</h1>\
        \<form method='post' action='/registro'>\
        \Nombre: <input name='nombre'><br>\
        \Email: <input name='email'><br>\
        \Contraseña: <input type='password' name='password'><br>\
        \<input type='submit' value='Registrar'>\
        \</form>\
        \<p><a href='/'>Volver al inicio</a></p>\
        \</body></html>"

    ("POST", "/registro") -> do
      (params, _) <- parseRequestBody lbsBackEnd req
      let lookupField k = maybe "" BS.unpack (lookup k params)
      executeInsertUser conn (lookupField "nombre") (lookupField "email") (lookupField "password")
      respond $ responseLBS status302 [("Location", "/login")] ""

    -----------------------------------------
    -- 👑 ADMINISTRACIÓN
    -----------------------------------------
    ("GET", "/admin") ->
      case currentUser of
        Just u | rol u == Admin ->
          respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] (renderBS (paginaAdmin usuarios))
        _ ->
          respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")]
            "<html><body><h1>🚫 Acceso denegado</h1><a href='/'>Volver al inicio</a></body></html>"

    ("GET", "/admin/nuevo") ->
      case currentUser of
        Just u | rol u == Admin ->
          respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] (renderBS (formUsuario "/admin/nuevo" Nothing))
        _ -> respond $ responseLBS status302 [("Location", "/")] ""

    ("POST", "/admin/nuevo") ->
      case currentUser of
        Just u | rol u == Admin -> do
          (params, _) <- parseRequestBody lbsBackEnd req
          let lookupField k = maybe "" BS.unpack (lookup k params)
              rolStr = lookupField "rol"
              nuevoRol = if rolStr == "Admin" then Admin else User
          us <- loadUsuarios conn
          let nuevo = Usuario (length us + 1) (lookupField "nombre") (lookupField "email") (lookupField "password") nuevoRol
          saveUsuarios conn (us ++ [nuevo])
          respond $ responseLBS status302 [("Location", "/admin")] ""
        _ -> respond $ responseLBS status302 [("Location", "/")] ""

    ("GET", p) | "/admin/editar/" `BS.isPrefixOf` p ->
      case (currentUser, takeIdAfter "/admin/editar/" p) of
        (Just u, Just uid) | rol u == Admin ->
          case find ((== uid) . userId) usuarios of
            Just usr ->
              respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")]
                (renderBS (formUsuario ("/admin/actualizar/" ++ show uid) (Just usr)))
            Nothing -> respond $ responseLBS status302 [("Location", "/admin")] ""
        _ -> respond $ responseLBS status302 [("Location", "/")] ""

    ("POST", p) | "/admin/actualizar/" `BS.isPrefixOf` p ->
      case (currentUser, takeIdAfter "/admin/actualizar/" p) of
        (Just u, Just uid) | rol u == Admin -> do
          (params, _) <- parseRequestBody lbsBackEnd req
          let lookupField k = maybe "" BS.unpack (lookup k params)
              rolStr = lookupField "rol"
              nuevoRol = if rolStr == "Admin" then Admin else User
          us <- loadUsuarios conn
          saveUsuarios conn $
            map (\usr -> if userId usr == uid
                         then usr { nombre = lookupField "nombre"
                                  , email = lookupField "email"
                                  , password = lookupField "password"
                                  , rol = nuevoRol }
                         else usr) us
          respond $ responseLBS status302 [("Location", "/admin")] ""
        _ -> respond $ responseLBS status302 [("Location", "/")] ""

    ("GET", p) | "/admin/borrar/" `BS.isPrefixOf` p ->
      case (currentUser, takeIdAfter "/admin/borrar/" p) of
        (Just u, Just uid) | rol u == Admin -> do
          us <- loadUsuarios conn
          saveUsuarios conn (filter ((/= uid) . userId) us)
          respond $ responseLBS status302 [("Location", "/admin")] ""
        _ -> respond $ responseLBS status302 [("Location", "/")] ""

    -----------------------------------------
    -- 🚗 GARAGE
    -----------------------------------------
    ("GET", "/garaje") ->
      case currentUser of
        Nothing -> respond $ responseLBS status302 [("Location", "/login")] ""
        Just u -> do
          let propios = filter (\v -> vehiculoOwnerId v == userId u) vehiculos
          respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] (renderBS (paginaLista propios))

    ("GET", "/garaje/nuevo") ->
      respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] (renderBS (formVehiculo "/garaje/nuevo" Nothing))

    ("POST", "/garaje/nuevo") ->
      case currentUser of
        Nothing -> respond $ responseLBS status302 [("Location", "/login")] ""
        Just u -> do
          (params, _) <- parseRequestBody lbsBackEnd req
          let lookupField k = maybe "" BS.unpack (lookup k params)
          vs <- loadVehiculos conn
          let nuevo = Vehiculo
                { vehiculoId        = nextId vs
                , vehiculoOwnerId   = userId u
                , tipo              = lookupField "tipo"
                , marca             = lookupField "marca"
                , modelo            = lookupField "modelo"
                , anio              = readDef 2025 (lookupField "anio")
                , color             = lookupField "color"
                , kilometros        = readDef 0 (lookupField "km")
                , ultimaRevision    = fromGregorian 2025 1 1
                , itvFecha          = Nothing
                , foto              = Nothing
                , notas             = lookupField "notas"
                }
          saveVehiculos conn (vs ++ [nuevo])
          respond $ responseLBS status302 [("Location", "/garaje")] ""

    ("GET", p) | "/garaje/" `BS.isPrefixOf` p && "/editar" `BS.isSuffixOf` p ->
      case (currentUser, takeIdAfter "/garaje/" p) of
        (Just u, Just vid) ->
          case find (\v -> vehiculoId v == vid && vehiculoOwnerId v == userId u) vehiculos of
            Just v -> respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")]
                        (renderBS (formVehiculo ("/garaje/" ++ show vid ++ "/actualizar") (Just v)))
            Nothing -> respond $ responseLBS status302 [("Location", "/garaje")] ""
        _ -> respond $ responseLBS status302 [("Location", "/login")] ""

    ("POST", p) | "/garaje/" `BS.isPrefixOf` p && "/actualizar" `BS.isSuffixOf` p ->
      case (currentUser, takeIdAfter "/garaje/" p) of
        (Just u, Just vid) -> do
          (params, _) <- parseRequestBody lbsBackEnd req
          let lookupField k = maybe "" BS.unpack (lookup k params)
          vs <- loadVehiculos conn
          let actualizar v = v
                { tipo        = lookupField "tipo"
                , marca       = lookupField "marca"
                , modelo      = lookupField "modelo"
                , anio        = readDef 2025 (lookupField "anio")
                , color       = lookupField "color"
                , kilometros  = readDef 0 (lookupField "km")
                , notas       = lookupField "notas"
                }
          saveVehiculos conn (map (\v -> if vehiculoId v == vid && vehiculoOwnerId v == userId u then actualizar v else v) vs)
          respond $ responseLBS status302 [("Location", "/garaje")] ""
        _ -> respond $ responseLBS status302 [("Location", "/login")] ""

    ("GET", p) | "/garaje/borrar/" `BS.isPrefixOf` p ->
      case (currentUser, takeIdAfter "/garaje/borrar/" p) of
        (Just u, Just vid) -> do
          vs <- loadVehiculos conn
          saveVehiculos conn (filter (\v -> vehiculoId v /= vid || vehiculoOwnerId v /= userId u) vs)
          respond $ responseLBS status302 [("Location", "/garaje")] ""
        _ -> respond $ responseLBS status302 [("Location", "/login")] ""

    ("GET", p) | "/garaje/" `BS.isPrefixOf` p ->
      case takeIdAfter "/garaje/" p of
        Just vid ->
          case find ((== vid) . vehiculoId) vehiculos of
            Just v  -> respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] (renderBS (paginaVehiculo v))
            Nothing -> respond $ responseLBS status302 [("Location", "/garaje")] ""
        Nothing -> respond $ responseLBS status302 [("Location", "/garaje")] ""

    -----------------------------------------
    -- ❌ RUTA INVÁLIDA
    -----------------------------------------
    _ -> respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] "Ruta no válida"

--------------------------------------------------------
-- 🧱 Helper para insertar usuarios nuevos
--------------------------------------------------------

executeInsertUser :: Connection -> String -> String -> String -> IO ()
executeInsertUser conn n e p = do
  us <- loadUsuarios conn
  let nuevo = Usuario (length us + 1) n e p User
  saveUsuarios conn (us ++ [nuevo])