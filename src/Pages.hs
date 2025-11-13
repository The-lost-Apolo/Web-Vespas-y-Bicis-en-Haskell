{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Pages where

import Lucid
import qualified Data.Text as T
import Models
import Users   -- 👈 para Usuario y Rol

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
      a_ [href_ (T.pack ("/garaje/borrar/" <> show (vehiculoId v)))] "🗑️ Borrar"

-- Página con render 3D + datos del vehículo
paginaVehiculo :: Vehiculo -> Html ()
paginaVehiculo v = do
  doctypehtml_ $ do
    head_ $ do
      title_ (toHtml (modelo v))
      meta_ [charset_ "UTF-8"]
      script_ [src_ "https://cdn.jsdelivr.net/npm/three@0.158.0/build/three.min.js"] ("" :: String)
      style_ (mconcat
        [ "body { background-color: #111; color: white; font-family: sans-serif; text-align: center; }"
        , "#cube-container { width: 300px; height: 300px; margin: 20px auto; }"
        , "a { color: lightgreen; text-decoration: none; margin: 0 8px; }"
        , "h1 { color: #4CAF50; }"
        , ".acciones { margin-top: 15px; }"
        ])
    body_ $ do
      h1_ (toHtml (marca v ++ " " ++ modelo v))
      div_ [id_ "cube-container"] mempty

      script_ (mconcat
        [ "const scene = new THREE.Scene();"
        , "const camera = new THREE.PerspectiveCamera(75, 1, 0.1, 1000);"
        , "const renderer = new THREE.WebGLRenderer({ antialias: true });"
        , "renderer.setSize(300, 300);"
        , "document.getElementById('cube-container').appendChild(renderer.domElement);"
        , "const geometry = new THREE.BoxGeometry();"
        , "const material = new THREE.MeshStandardMaterial({ color: 0x00ff00 });"
        , "const cube = new THREE.Mesh(geometry, material);"
        , "scene.add(cube);"
        , "const light = new THREE.PointLight(0xffffff, 1, 100);"
        , "light.position.set(5, 5, 5);"
        , "scene.add(light);"
        , "camera.position.z = 2;"
        , "function animate() { requestAnimationFrame(animate); cube.rotation.x += 0.01; cube.rotation.y += 0.01; renderer.render(scene, camera); }"
        , "animate();"
        ])

      h2_ "Detalles del vehículo"
      p_ (toHtml ("Tipo: " ++ tipo v))
      p_ (toHtml ("Año: " ++ show (anio v)))
      p_ (toHtml ("Color: " ++ color v))
      p_ (toHtml ("Kilómetros: " ++ show (kilometros v)))
      p_ (toHtml ("Notas: " ++ notas v))

      div_ [class_ "acciones"] $ do
        a_ [href_ (T.pack ("/garaje/" ++ show (vehiculoId v) ++ "/editar"))] "✏️ Editar"
        a_ [href_ (T.pack ("/garaje/borrar/" ++ show (vehiculoId v)))] "🗑️ Borrar"
        a_ [href_ "/garaje"] "⬅️ Volver al garaje"

-- Formulario de alta/edición de vehículo
formVehiculo :: String -> Maybe Vehiculo -> Html ()
formVehiculo actionPath maybeV = html_ $ do
  head_ $ do
    meta_ [charset_ "UTF-8"]
    title_ "Formulario vehículo"
    style_ "body { background: #111; color: white; font-family: sans-serif; } input, textarea, select { margin: 4px; }"
  body_ $ do
    h1_ (if maybeV == Nothing then "Añadir vehículo" else "Editar vehículo")
    form_ [method_ "post", action_ (T.pack actionPath)] $ do
      p_ $ do
        "Tipo: "
        select_ [name_ "tipo"] $ do
          option_ "Vespa"
          option_ "Bici"
      p_ $ do "Marca: " >> input_ [type_ "text", name_ "marca", value_ (maybe "" (T.pack . marca) maybeV)]
      p_ $ do "Modelo: " >> input_ [type_ "text", name_ "modelo", value_ (maybe "" (T.pack . modelo) maybeV)]
      p_ $ do "Año: " >> input_ [type_ "number", name_ "anio", value_ (T.pack (show (maybe 2025 anio maybeV)))]
      p_ $ do "Color: " >> input_ [type_ "text", name_ "color", value_ (maybe "" (T.pack . color) maybeV)]
      p_ $ do "Kilómetros: " >> input_ [type_ "number", name_ "km", value_ (T.pack (show (maybe 0 kilometros maybeV)))]
      p_ $ do "Notas: " >> textarea_ [name_ "notas"] (toHtml (maybe "" notas maybeV))
      p_ $ input_ [type_ "submit", value_ "Guardar"]
      -- 🆕 Botón para volver al vehículo o al garaje
      p_ $ do
        let volverLink = case maybeV of
              Just v  -> "/garaje/" <> show (vehiculoId v)
              Nothing -> "/garaje"
        a_ [href_ (T.pack volverLink)] "⬅️ Volver al vehículo"

-- 🛠️ Página del panel de administración
paginaAdmin :: [Usuario] -> Html ()
paginaAdmin usuarios = html_ $ do
  head_ $ do
    meta_ [charset_ "UTF-8"]
    title_ "Panel de Administración"
    style_
      "body { background: #111; color: white; font-family: sans-serif; }\
      \a { color: lightgreen; }\
      \table { width: 100%; border-collapse: collapse; margin-top: 20px; }\
      \th, td { border: 1px solid #333; padding: 8px; text-align: left; }"
  body_ $ do
    h1_ "🛠️ Panel de Administración"
    a_ [href_ "/"] "🏠 Volver al inicio"
    " | "
    a_ [href_ "/admin/nuevo"] "➕ Crear nuevo usuario"
    table_ $ do
      tr_ $ do
        th_ "ID"
        th_ "Nombre"
        th_ "Email"
        th_ "Rol"
        th_ "Acciones"
      mapM_ renderUser usuarios

-- 👇 Tipo explícito: evita la ambigüedad del compilador
renderUser :: Usuario -> Html ()
renderUser u = tr_ $ do
  td_ (toHtml (show (userId u)))
  td_ (toHtml (nombre u))
  td_ (toHtml (email u))
  td_ (toHtml (show (rol u)))
  td_ $ do
    a_ [href_ (T.pack ("/admin/editar/" <> show (userId u)))] "✏️ Editar"
    " | "
    a_ [href_ (T.pack ("/admin/borrar/" <> show (userId u)))] "🗑️ Borrar"

-- 📝 Formulario para crear o editar usuarios
formUsuario :: String -> Maybe Usuario -> Html ()
formUsuario actionPath maybeU = html_ $ do
  head_ $ do
    meta_ [charset_ "UTF-8"]
    title_ "Formulario Usuario"
    style_ "body { background: #111; color: white; font-family: sans-serif; } input, select { margin: 4px; }"
  body_ $ do
    h1_ (if maybeU == Nothing then "Crear nuevo usuario" else "Editar usuario")
    form_ [method_ "post", action_ (T.pack actionPath)] $ do
      p_ $ do "Nombre: " >> input_ [type_ "text", name_ "nombre", value_ (maybe "" (T.pack . nombre) maybeU)]
      p_ $ do "Email: " >> input_ [type_ "email", name_ "email", value_ (maybe "" (T.pack . email) maybeU)]
      p_ $ do "Contraseña: " >> input_ [type_ "password", name_ "password", value_ ""]
      p_ $ do
        "Rol: "
        select_ [name_ "rol"] $ do
          option_ [value_ "User"] "User"
          option_ [value_ "Admin"] "Admin"
      p_ $ input_ [type_ "submit", value_ "Guardar"]
      p_ $ a_ [href_ "/admin"] "⬅️ Volver al panel"


-- Pagina Rutas
paginaRutas :: Html ()
paginaRutas = html_ $ do
  head_ $ do
    meta_ [charset_ "UTF-8"]
    title_ "Rutas"
    style_ "body { background:#111; color:white; font-family:sans-serif; text-align:center; } a{color:lightgreen; margin:10px;}"
  body_ $ do
    h1_ "🗺️ Rutas"
    a_ [href_ "/"] "🏠 Inicio"
    a_ [href_ "/rutas/iniciar"] "▶️ Iniciar nueva ruta"
    a_ [href_ "/rutas/historial"] "📜 Ver historial"


paginaIniciarRuta :: Html ()
paginaIniciarRuta = html_ $ do
  head_ $ do
    meta_ [charset_ "UTF-8"]
    title_ "Iniciar ruta"
    link_ [ rel_ "stylesheet"
          , href_ "https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"]
    script_ [ src_ "https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"] ("" :: String)
    style_ "body{background:#111;color:white;font-family:sans-serif;text-align:center;}\
           \#map{height:400px;margin:20px;}\
           \button{margin:10px;padding:10px 15px;font-size:16px;border-radius:8px;}"

  body_ $ do
    h1_ "📍 Iniciando ruta..."
    div_ [id_ "map"] mempty
    p_ [id_ "stats"] "Distancia: 0 m | Velocidad: 0 km/h | Tiempo: 0 s"
    button_ [id_ "stopBtn"] "⏹️ Terminar ruta"
    button_ [id_ "pauseBtn"] "⏸️ Pausar"

    script_ $ mconcat
      [

      ------------------------------------------------------------------------------
      -- MAPA Y VARIABLES
      ------------------------------------------------------------------------------
      "let map = L.map('map').setView([0,0], 13);\n"
      , "L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',{maxZoom:19}).addTo(map);\n"
      , "let track = L.polyline([], {color:'lime'}).addTo(map);\n"

      , "let totalDist = 0;\n"
      , "let lastPos = null;\n"
      , "let lastTime = null;\n"
      , "let startTime = Date.now();\n"
      , "let gotRealGPS = false;\n"

      ------------------------------------------------------------------------------
      -- FUNCIÓN PRINCIPAL: CALCULA DISTANCIA, VELOCIDAD, TIEMPO
      ------------------------------------------------------------------------------
      , "function updatePos(pos) {\n"
      , "  let now = Date.now();\n"
      , "  let lat = pos.coords.latitude;\n"
      , "  let lon = pos.coords.longitude;\n"

      -- PRIMER PUNTO
      , "  if (!lastPos) {\n"
      , "    lastPos = {lat, lon};\n"
      , "    lastTime = now;\n"
      , "    map.setView([lat,lon], 16);\n"
      , "    track.addLatLng([lat,lon]);\n"
      , "    return;\n"
      , "  }\n"

      -- DISTANCIA ENTRE ÚLTIMO PUNTO Y EL ACTUAL
      , "  let d = map.distance([lat,lon], [lastPos.lat,lastPos.lon]);\n"

      -- TIEMPO ENTRE MEDICIONES
      , "  let dt = (now - lastTime) / 1000;\n"

      -- SUMA DISTANCIA SOLO SI ES REAL (> 0.5m)
      , "  if (d > 0.5) {\n"
      , "    totalDist += d;\n"
      , "    track.addLatLng([lat, lon]);\n"
      , "  }\n"

      -- VELOCIDAD REAL
      , "  let v = dt > 0 ? (d / dt) * 3.6 : 0;\n"

      -- ACTUALIZAR ESTADO
      , "  lastPos = {lat, lon};\n"
      , "  lastTime = now;\n"

      -- ACTUALIZAR UI
      , "  let totalTime = (now - startTime) / 1000;\n"
      , "  document.getElementById('stats').innerText =\n"
      , "    `Distancia: ${totalDist.toFixed(1)} m | Velocidad: ${v.toFixed(1)} km/h | Tiempo: ${totalTime.toFixed(1)} s`;\n"
      , "}\n"

      ------------------------------------------------------------------------------
      -- GPS REAL
      ------------------------------------------------------------------------------
      , "navigator.geolocation.watchPosition(p => {\n"
      , "  gotRealGPS = true;\n"
      , "  updatePos(p);\n"
      , "}, err => console.warn('⚠️ Error GPS real:', err));\n"

      ------------------------------------------------------------------------------
      -- SIMULADOR DESPUÉS DE 5s (SI NO HAY GPS REAL)
      ------------------------------------------------------------------------------
      , "setTimeout(() => {\n"
      , "  if (gotRealGPS) {\n"
      , "    console.log('🛰️ GPS real detectado, sin simulación'); return;\n"
      , "  }\n"

      , "  console.warn('⚙️ Activando simulador GPS recto...');\n"

      -- REEMPLAZAMOS WATCHPOSITION PARA REGISTRAR CALLBACKS
      , "  window.geoSim = { callbacks: [updatePos] };\n"
      , "  navigator.geolocation.watchPosition = function(cb) {\n"
      , "    window.geoSim.callbacks.push(cb);\n"
      , "    console.log('📡 Simulación: callback registrada');\n"
      , "  };\n"

      ------------------------------------------------------------------------------
      -- 🔥 RUTA RECTILÍNEA HACIA EL NORTE (≈5.5m POR PUNTO)
      ------------------------------------------------------------------------------
      , "  let path = [];\n"
      , "  let lat = 40.416800;\n"
      , "  let lon = -3.703800;\n"

      , "  for (let j = 0; j < 500; j++) {\n"
      , "    lat += 0.00005;   // ≈5.5 metros hacia el norte\n"
      , "    path.push([lat, lon]);\n"
      , "  }\n"

      -- EJECUCIÓN SIMULADA
      , "  let idx = 0;\n"
      , "  setInterval(() => {\n"
      , "    const pos = { coords: {\n"
      , "      latitude: path[idx][0],\n"
      , "      longitude: path[idx][1],\n"
      , "      speed: null\n"
      , "    }};\n"

      , "    window.geoSim.callbacks.forEach(cb => cb(pos));\n"
      , "    idx = (idx + 1) % path.length;\n"
      , "  }, 2000);\n"

      , "}, 5000);\n"
      ]


-- 📜 Historial de rutas
paginaHistorial :: [Ruta] -> Html ()
paginaHistorial rutas = html_ $ do
  head_ $ do
    meta_ [charset_ "UTF-8"]
    title_ "Historial de rutas"
    style_
      "body { background:#111; color:white; font-family:sans-serif; text-align:center; }\
      \a{color:lightgreen;margin:8px;}\
      \table {margin:auto; border-collapse:collapse; margin-top:20px;}\
      \th, td {border:1px solid #333; padding:6px 12px;}"
  body_ $ do
    h1_ "📜 Historial de rutas"
    a_ [href_ "/rutas"] "⬅️ Volver a Rutas"
    table_ $ do
      tr_ $ do
        th_ "ID"
        th_ "Fecha"
        th_ "Distancia (m)"
        th_ "Duración (s)"
        th_ "Velocidad media (km/h)"
      mapM_ renderRuta rutas

renderRuta :: Ruta -> Html ()
renderRuta r = tr_ $ do
  td_ (toHtml (show (rutaId r)))
  td_ (toHtml (rutaFecha r))
  td_ (toHtml (show (rutaDistancia r)))
  td_ (toHtml (show (rutaDuracion r)))
  td_ (toHtml (show (rutaVelMedia r)))



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
