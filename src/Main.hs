{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status302)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import Control.Concurrent.MVar
import Data.List (find)
import Data.Time (fromGregorian)
import Lucid (renderBS)
import Text.Read (readMaybe)
import Data.Char (isDigit)

import Pages
import Models
import Users

main :: IO ()
main = do
  vehiculosDB <- newMVar []
  usuariosDB <- newMVar [Usuario 1 "Admin" "admin@example.com" "admin" Admin]
  usuarioActual <- newMVar Nothing
  putStrLn "Servidor iniciado en http://localhost:8080"
  run 8080 (app vehiculosDB usuariosDB usuarioActual)

app :: MVar [Vehiculo] -> MVar [Usuario] -> MVar (Maybe Usuario) -> Application
app vehiculosDB usuariosDB usuarioActual req respond = do
  vehiculos <- readMVar vehiculosDB
  usuarios <- readMVar usuariosDB
  currentUser <- readMVar usuarioActual
  let path = rawPathInfo req

  case (requestMethod req, path) of

    -----------------------------------------
    -- 🏠 Página de inicio
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
      let lookupField key = maybe "" BS.unpack (lookup key params)
          email = lookupField "email"
          pwd = lookupField "password"
      case find (\u -> email == Users.email u && pwd == password u) usuarios of
        Just u -> do
          swapMVar usuarioActual (Just u)
          respond $ responseLBS status302 [("Location", "/garaje")] ""
        Nothing ->
          respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] $
            "<!DOCTYPE html><html><head><meta charset='UTF-8'><title>Error de login</title>\
            \<style>body{background:#111;color:white;font-family:sans-serif;text-align:center;}\
            \a{color:lightgreen;text-decoration:none;margin:10px;display:inline-block;}</style>\
            \</head><body>\
            \<h1>⚠️ Usuario o contraseña incorrectos</h1>\
            \<p>El usuario no existe o la contraseña es incorrecta.</p>\
            \<p>\
            \<a href='/registro'>📝 Crear cuenta</a>\
            \<a href='/'>🏠 Volver al inicio</a>\
            \</p>\
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
      let lookupField key = maybe "" BS.unpack (lookup key params)
      modifyMVar_ usuariosDB $ \us -> do
        let nuevo = Usuario (length us + 1) (lookupField "nombre") (lookupField "email") (lookupField "password") User
        return (us ++ [nuevo])
      respond $ responseLBS status302 [("Location", "/login")] ""

    -----------------------------------------
    -- 👑 PANEL DE ADMINISTRACIÓN
    -----------------------------------------
    ("GET", "/admin") ->
      case currentUser of
        Just u | rol u == Admin -> do
          respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] (renderBS (paginaAdmin usuarios))
        _ -> respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] $
          "<html><body><h1>🚫 Acceso denegado</h1><p>Solo el administrador puede ver esta página.</p><a href='/'>Volver al inicio</a></body></html>"

    ("GET", "/admin/nuevo") ->
      case currentUser of
        Just u | rol u == Admin ->
          respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] (renderBS (formUsuario "/admin/nuevo" Nothing))
        _ -> respond $ responseLBS status302 [("Location", "/")] ""

    ("POST", "/admin/nuevo") ->
      case currentUser of
        Just u | rol u == Admin -> do
          (params, _) <- parseRequestBody lbsBackEnd req
          let lookupField key = maybe "" BS.unpack (lookup key params)
              rolStr = lookupField "rol"
              nuevoRol = if rolStr == "Admin" then Admin else User
              nuevo = Usuario (length usuarios + 1) (lookupField "nombre") (lookupField "email") (lookupField "password") nuevoRol
          modifyMVar_ usuariosDB (return . (++ [nuevo]))
          respond $ responseLBS status302 [("Location", "/admin")] ""
        _ -> respond $ responseLBS status302 [("Location", "/")] ""

    ("GET", p) | "/admin/editar/" `BS.isPrefixOf` p ->
      case (currentUser, takeIdAfter "/admin/editar/" p) of
        (Just u, Just uid) | rol u == Admin ->
          case find ((== uid) . userId) usuarios of
            Just usr -> respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] (renderBS (formUsuario ("/admin/actualizar/" ++ show uid) (Just usr)))
            Nothing -> respond $ responseLBS status302 [("Location", "/admin")] ""
        _ -> respond $ responseLBS status302 [("Location", "/")] ""

    ("POST", p) | "/admin/actualizar/" `BS.isPrefixOf` p ->
      case (currentUser, takeIdAfter "/admin/actualizar/" p) of
        (Just u, Just uid) | rol u == Admin -> do
          (params, _) <- parseRequestBody lbsBackEnd req
          let lookupField key = maybe "" BS.unpack (lookup key params)
              rolStr = lookupField "rol"
              nuevoRol = if rolStr == "Admin" then Admin else User
          modifyMVar_ usuariosDB $ \us ->
            return (map (\usr -> if userId usr == uid
                                 then usr { nombre = lookupField "nombre"
                                          , email = lookupField "email"
                                          , password = lookupField "password"
                                          , rol = nuevoRol }
                                 else usr) us)
          respond $ responseLBS status302 [("Location", "/admin")] ""
        _ -> respond $ responseLBS status302 [("Location", "/")] ""

    ("GET", p) | "/admin/borrar/" `BS.isPrefixOf` p ->
      case (currentUser, takeIdAfter "/admin/borrar/" p) of
        (Just u, Just uid) | rol u == Admin -> do
          modifyMVar_ usuariosDB (return . filter ((/= uid) . userId))
          respond $ responseLBS status302 [("Location", "/admin")] ""
        _ -> respond $ responseLBS status302 [("Location", "/")] ""

    -----------------------------------------
    -- 🚗 GARAGE (protegido)
    -----------------------------------------
    ("GET", "/garaje") ->
      case currentUser of
        Nothing -> respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] $
          "<html><head><meta charset='UTF-8'><title>Acceso restringido</title></head><body>\
          \<h1>🔒 Debes iniciar sesión para acceder al garaje</h1>\
          \<p><a href='/login'>Iniciar sesión</a> | <a href='/registro'>Registrarme</a></p>\
          \<p><a href='/'>Volver al inicio</a></p>\
          \</body></html>"
        Just u -> do
          let propios = filter (\v -> vehiculoOwnerId v == userId u) vehiculos
          respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] (renderBS (paginaLista propios))

    -----------------------------------------
    -- 🚘 NUEVO VEHÍCULO
    -----------------------------------------
    ("GET", "/garaje/nuevo") ->
      respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] (renderBS (formVehiculo "/garaje/nuevo" Nothing))

    ("POST", "/garaje/nuevo") ->
      case currentUser of
        Nothing -> respond $ responseLBS status302 [("Location", "/login")] ""
        Just u -> do
          (params, _) <- parseRequestBody lbsBackEnd req
          let lookupField key = maybe "" BS.unpack (lookup key params)
              nuevo = Vehiculo
                { vehiculoId = nextId vehiculos
                , vehiculoOwnerId = userId u
                , tipo = lookupField "tipo"
                , marca = lookupField "marca"
                , modelo = lookupField "modelo"
                , anio = readDef 2025 (lookupField "anio")
                , color = lookupField "color"
                , kilometros = readDef 0 (lookupField "km")
                , ultimaRevision = fromGregorian 2025 1 1
                , itvFecha = Nothing
                , foto = Nothing
                , notas = lookupField "notas"
                }
          modifyMVar_ vehiculosDB (return . (++ [nuevo]))
          respond $ responseLBS status302 [("Location", "/garaje")] ""

    -----------------------------------------
    -- ✏️ EDITAR VEHÍCULO
    -----------------------------------------
    ("GET", p) | "/garaje/" `BS.isPrefixOf` p && "/editar" `BS.isSuffixOf` p ->
      case (currentUser, takeIdAfter "/garaje/" p) of
        (Just u, Just vid) ->
          case find (\v -> vehiculoId v == vid && vehiculoOwnerId v == userId u) vehiculos of
            Just v ->
              respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")]
                (renderBS (formVehiculo ("/garaje/" ++ show vid ++ "/actualizar") (Just v)))
            Nothing ->
              respond $ responseLBS status302 [("Location", "/garaje")] ""
        _ ->
          respond $ responseLBS status302 [("Location", "/login")] ""

    -----------------------------------------
    -- 💾 ACTUALIZAR VEHÍCULO
    -----------------------------------------
    ("POST", p) | "/garaje/" `BS.isPrefixOf` p && "/actualizar" `BS.isSuffixOf` p ->
      case (currentUser, takeIdAfter "/garaje/" p) of
        (Just u, Just vid) -> do
          (params, _) <- parseRequestBody lbsBackEnd req
          let lookupField key = maybe "" BS.unpack (lookup key params)
          modifyMVar_ vehiculosDB $ \vs -> do
            let actualizar v = v
                  { tipo        = lookupField "tipo"
                  , marca       = lookupField "marca"
                  , modelo      = lookupField "modelo"
                  , anio        = readDef 2025 (lookupField "anio")
                  , color       = lookupField "color"
                  , kilometros  = readDef 0 (lookupField "km")
                  , notas       = lookupField "notas"
                  }
            return (map (\v -> if vehiculoId v == vid && vehiculoOwnerId v == userId u
                               then actualizar v else v) vs)
          respond $ responseLBS status302 [("Location", "/garaje")] ""
        _ -> respond $ responseLBS status302 [("Location", "/login")] ""

    -----------------------------------------
    -- 🗑️ BORRAR VEHÍCULO
    -----------------------------------------
    ("GET", p) | "/garaje/borrar/" `BS.isPrefixOf` p ->
      case (currentUser, takeIdAfter "/garaje/borrar/" p) of
        (Just u, Just vid) -> do
          modifyMVar_ vehiculosDB (return . filter (\v -> vehiculoId v /= vid || vehiculoOwnerId v /= userId u))
          respond $ responseLBS status302 [("Location", "/garaje")] ""
        _ -> respond $ responseLBS status302 [("Location", "/login")] ""

    -----------------------------------------
    -- 👁️ VER VEHÍCULO
    -----------------------------------------
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
  _ -> def
