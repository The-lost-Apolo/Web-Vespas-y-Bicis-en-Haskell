{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status302)
import qualified Data.ByteString.Char8 as BS
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import Control.Concurrent.MVar
import Data.List (find)
import Data.Time (fromGregorian)
import Lucid (renderBS)
import Text.Read (readMaybe)
import Data.Char (isDigit)

import Pages
import Models

main :: IO ()
main = do
  db <- newMVar []
  putStrLn "Servidor iniciado en http://localhost:8080"
  run 8080 (app db)

app :: MVar [Vehiculo] -> Application
app db req respond = do
  vehiculos <- readMVar db
  let path = rawPathInfo req
  case (requestMethod req, path) of

    -- Página de inicio
    ("GET", "/") ->
      respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] $
        "<!DOCTYPE html><html><head><meta charset='UTF-8'><title>Inicio</title>\
        \<style>body{background:#111;color:white;font-family:sans-serif;}a{color:lightgreen;}</style>\
        \</head><body><h1>Bienvenido a Mi Garaje Vespa 🚗</h1>\
        \<a href='/garaje'>Entrar al garaje</a></body></html>"

    -- Página del garaje (listado)
    ("GET", "/garaje") ->
      respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] (renderBS (paginaLista vehiculos))

    -- Formulario: nuevo
    ("GET", "/garaje/nuevo") ->
      respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] (renderBS (formVehiculo "/garaje/nuevo" Nothing))

    -- Crear vehículo (POST)
    ("POST", "/garaje/nuevo") -> do
      (params, _) <- parseRequestBody lbsBackEnd req
      let lookupField key = maybe "" BS.unpack (lookup key params)
          nuevo = Vehiculo
            { vehiculoId = nextId vehiculos
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
      modifyMVar_ db (return . (++ [nuevo]))
      respond $ responseLBS status302 [("Location", "/garaje")] ""

    -- Borrar vehículo
    ("GET", p) | "/garaje/borrar/" `BS.isPrefixOf` p ->
      case takeIdAfter "/garaje/borrar/" p of
        Just vid -> do
          modifyMVar_ db (return . filter ((/= vid) . vehiculoId))
          respond $ responseLBS status302 [("Location", "/garaje")] ""
        Nothing ->
          respond $ responseLBS status302 [("Location", "/garaje")] ""

    -- Formulario editar
    ("GET", p) | "/garaje/" `BS.isPrefixOf` p && "/editar" `BS.isSuffixOf` p ->
      case takeIdAfter "/garaje/" p of
        Just vid ->
          case find ((== vid) . vehiculoId) vehiculos of
            Just v  -> respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")]
                                    (renderBS (formVehiculo ("/garaje/" ++ show vid ++ "/actualizar") (Just v)))
            Nothing -> respond $ responseLBS status302 [("Location", "/garaje")] ""
        Nothing -> respond $ responseLBS status302 [("Location", "/garaje")] ""

    -- Actualizar vehículo
    ("POST", p) | "/garaje/" `BS.isPrefixOf` p && "/actualizar" `BS.isSuffixOf` p ->
      case takeIdAfter "/garaje/" p of
        Just vid -> do
          (params, _) <- parseRequestBody lbsBackEnd req
          let lookupField key = maybe "" BS.unpack (lookup key params)
          modifyMVar_ db $ \vs -> do
            let actualizar v = v
                  { tipo        = lookupField "tipo"
                  , marca       = lookupField "marca"
                  , modelo      = lookupField "modelo"
                  , anio        = readDef 2025 (lookupField "anio")
                  , color       = lookupField "color"
                  , kilometros  = readDef 0 (lookupField "km")
                  , notas       = lookupField "notas"
                  }
            return (map (\v -> if vehiculoId v == vid then actualizar v else v) vs)
          respond $ responseLBS status302 [("Location", "/garaje")] ""
        Nothing ->
          respond $ responseLBS status302 [("Location", "/garaje")] ""

    -- Ver vehículo
    ("GET", p) | "/garaje/" `BS.isPrefixOf` p ->
      case takeIdAfter "/garaje/" p of
        Just vid ->
          case find ((== vid) . vehiculoId) vehiculos of
            Just v  -> respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] (renderBS (paginaVehiculo v))
            Nothing -> respond $ responseLBS status302 [("Location", "/garaje")] ""
        Nothing -> respond $ responseLBS status302 [("Location", "/garaje")] ""

    -- Archivos estáticos
    _ | "/static/" `BS.isPrefixOf` path -> do
          let file = drop 8 (BS.unpack path)
          respond $ responseFile status200 [("Content-Type", "application/javascript")] ("static/" ++ file) Nothing

    -- Fallback
    _ -> respond $ responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] "Ruta no válida"

-- Helper: leer ID después de un prefijo
takeIdAfter :: String -> BS.ByteString -> Maybe Int
takeIdAfter prefix p =
  let rest = drop (length prefix) (BS.unpack p)
      digits = takeWhile isDigit rest
  in if null digits then Nothing else readMaybe digits

-- Conversión segura de texto a número
readDef :: Read a => a -> String -> a
readDef def s = case reads s of
  [(x, "")] -> x
  _ -> def