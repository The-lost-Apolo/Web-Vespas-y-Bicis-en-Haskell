{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Pages where

import Lucid
import qualified Data.Text as T
import Models

-- Página que muestra la lista de vehículos
paginaLista :: [Vehiculo] -> Html ()
paginaLista vehiculos = html_ $ do
  head_ $ do
    meta_ [charset_ "UTF-8"]
    title_ "Mi Garaje"
    style_ "body { background: #111; color: white; font-family: sans-serif; } a { color: lightgreen; }"
  body_ $ do
    a_ [href_ "/"] "🏠 Volver al inicio"
    h1_ "🚗 Mi Garaje"
    a_ [href_ "/garaje/nuevo"] "➕ Añadir vehículo"
    hr_ []
    ul_ $ mapM_ renderItem vehiculos
  where
    renderItem v = li_ $ do
      toHtml (marca v <> " " <> modelo v <> " (" <> tipo v <> ") - " <> color v)
      " — "
      a_ [href_ (T.pack ("/garaje/" <> show (vehiculoId v)))] "Ver"
      " | "
      a_ [href_ (T.pack ("/garaje/borrar/" <> show (vehiculoId v)))] "Borrar"

-- Página con render 3D + datos del vehículo
paginaVehiculo :: Vehiculo -> Html ()
paginaVehiculo v = html_ $ do
  head_ $ do
    meta_ [charset_ "UTF-8"]
    title_ (toHtml (marca v <> " " <> modelo v))
    script_ [src_ "/static/three.min.js"] ("" :: String)
    style_ "body { margin: 0; background: #111; color: white; font-family: sans-serif; }"
  body_ $ do
    script_ (T.pack escena3D)
    h1_ (toHtml (marca v <> " " <> modelo v))
    ul_ $ do
      li_ $ "Tipo: " >> toHtml (tipo v)
      li_ $ "Año: " >> toHtml (show (anio v))
      li_ $ "Color: " >> toHtml (color v)
      li_ $ "Kilómetros: " >> toHtml (show (kilometros v))
      li_ $ "Notas: " >> toHtml (notas v)
    p_ $ do
      a_ [href_ (T.pack ("/garaje/" <> show (vehiculoId v) <> "/editar"))] "✏️ Editar datos"
      " | "
      a_ [href_ "/garaje"] "⬅️ Volver al garaje"

-- Formulario de alta/edición
formVehiculo :: String -> Maybe Vehiculo -> Html ()
formVehiculo actionPath maybeV = html_ $ do
  head_ $ do
    meta_ [charset_ "UTF-8"]
    title_ "Formulario vehículo"
    style_ "body { background: #111; color: white; font-family: sans-serif; } input, textarea, select { margin: 4px; }"
  body_ $ do
    h1_ (if maybeV == Nothing then "Añadir vehículo" else "Editar vehículo")
    form_ [method_ "post", action_ (T.pack actionPath)] $ do
      p_ $ do "Tipo: " >> select_ [name_ "tipo"] (do option_ "Vespa"; option_ "Bici")
      p_ $ do "Marca: " >> input_ [type_ "text", name_ "marca", value_ (maybe "" (T.pack . marca) maybeV)]
      p_ $ do "Modelo: " >> input_ [type_ "text", name_ "modelo", value_ (maybe "" (T.pack . modelo) maybeV)]
      p_ $ do "Año: " >> input_ [type_ "number", name_ "anio", value_ (T.pack (show (maybe 2025 anio maybeV)))]
      p_ $ do "Color: " >> input_ [type_ "text", name_ "color", value_ (maybe "" (T.pack . color) maybeV)]
      p_ $ do "Kilómetros: " >> input_ [type_ "number", name_ "km", value_ (T.pack (show (maybe 0 kilometros maybeV)))]
      p_ $ do "Notas: " >> textarea_ [name_ "notas"] (toHtml (maybe "" notas maybeV))
      p_ $ input_ [type_ "submit", value_ "Guardar"]

-- Cubo 3D de prueba
escena3D :: String
escena3D = unlines
  [ "const scene = new THREE.Scene();"
  , "const camera = new THREE.PerspectiveCamera(75, window.innerWidth/window.innerHeight, 0.1, 1000);"
  , "const renderer = new THREE.WebGLRenderer({antialias: true});"
  , "renderer.setSize(window.innerWidth, window.innerHeight/2);"
  , "document.body.appendChild(renderer.domElement);"
  , "const geometry = new THREE.BoxGeometry();"
  , "const material = new THREE.MeshStandardMaterial({ color: 0x00ff00 });"
  , "const cube = new THREE.Mesh(geometry, material);"
  , "scene.add(cube);"
  , "const light = new THREE.PointLight(0xffffff, 1, 100);"
  , "light.position.set(10, 10, 10);"
  , "scene.add(light);"
  , "camera.position.z = 5;"
  , "function animate() {"
  , "  requestAnimationFrame(animate);"
  , "  cube.rotation.x += 0.01;"
  , "  cube.rotation.y += 0.01;"
  , "  renderer.render(scene, camera);"
  , "}"
  , "animate();"
  ]