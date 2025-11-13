{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Pages where

import Lucid
import qualified Data.Text as T
import Models
import Users   -- Usuario, Rol


------------------------------------------------------------
-- 🚗 LISTA DE VEHÍCULOS
------------------------------------------------------------
paginaLista :: [Vehiculo] -> Html ()
paginaLista vehiculos = html_ $ do
  head_ $ do
    meta_ [charset_ "UTF-8"]
    title_ "Mi Garaje"
    style_ "body { background: #111; color: white; font-family: sans-serif; } \
           \a { color: lightgreen; }"
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


------------------------------------------------------------
-- 🚗 PÁGINA DE VEHÍCULO (THREE.JS)
------------------------------------------------------------
paginaVehiculo :: Vehiculo -> Html ()
paginaVehiculo v = do
  doctypehtml_ $ do
    head_ $ do
      title_ (toHtml (modelo v))
      meta_ [charset_ "UTF-8"]
      script_ [src_ "https://cdn.jsdelivr.net/npm/three@0.158.0/build/three.min.js"] ("" :: String)
      style_ "body { background-color: #111; color: white; font-family: sans-serif; text-align: center; } \
             \#cube-container { width: 300px; height: 300px; margin: 20px auto; } \
             \a { color: lightgreen; text-decoration:none; margin:8px; } \
             \h1 { color:#4CAF50; }"
    body_ $ do
      h1_ (toHtml (marca v ++ " " ++ modelo v))
      div_ [id_ "cube-container"] mempty

      script_ escena3D

      h2_ "Detalles del vehículo"
      p_ (toHtml ("Tipo: " ++ tipo v))
      p_ (toHtml ("Año: " ++ show (anio v)))
      p_ (toHtml ("Color: " ++ color v))
      p_ (toHtml ("Kilómetros: " ++ show (kilometros v)))
      p_ (toHtml ("Notas: " ++ notas v))

      a_ [href_ (T.pack ("/garaje/" ++ show (vehiculoId v) ++ "/editar"))] "✏️ Editar"
      a_ [href_ (T.pack ("/garaje/borrar/" ++ show (vehiculoId v)))] "🗑️ Borrar"
      a_ [href_ "/garaje"] "⬅️ Volver"


------------------------------------------------------------
-- 🚗 FORMULARIO VEHÍCULO
------------------------------------------------------------
formVehiculo :: String -> Maybe Vehiculo -> Html ()
formVehiculo actionPath maybeV = html_ $ do
  head_ $ do
    meta_ [charset_ "UTF-8"]
    title_ "Formulario vehículo"
    style_ "body { background:#111; color:white; font-family:sans-serif; } \
           \input, textarea, select { margin:4px; }"

  body_ $ do
    h1_ (if maybeV == Nothing then "Añadir vehículo" else "Editar vehículo")

    form_ [method_ "post", action_ (T.pack actionPath)] $ do

      p_ $ do "Tipo: "
              select_ [name_ "tipo"] $ do
                option_ "Vespa"
                option_ "Bici"

      p_ $ do "Marca: "
              input_ [type_ "text", name_ "marca", value_ (maybe "" (T.pack . marca) maybeV)]

      p_ $ do "Modelo: "
              input_ [type_ "text", name_ "modelo", value_ (maybe "" (T.pack . modelo) maybeV)]

      p_ $ do "Año: "
              input_ [type_ "number", name_ "anio", value_ (T.pack (show (maybe 2025 anio maybeV)))]

      p_ $ do "Color: "
              input_ [type_ "text", name_ "color", value_ (maybe "" (T.pack . color) maybeV)]

      p_ $ do "Kilómetros: "
              input_ [type_ "number", name_ "km", value_ (T.pack (show (maybe 0 kilometros maybeV)))]

      p_ $ do "Notas: "
              textarea_ [name_ "notas"] (toHtml (maybe "" notas maybeV))

      p_ $ input_ [type_ "submit", value_ "Guardar"]

      p_ $ do
        let volver =
              case maybeV of
                Just v  -> "/garaje/" <> show (vehiculoId v)
                Nothing -> "/garaje"
        a_ [href_ (T.pack volver)] "⬅️ Volver"

------------------------------------------------------------
-- 👑 ADMIN
------------------------------------------------------------
paginaAdmin :: [Usuario] -> Html ()
paginaAdmin usuarios = html_ $ do
  head_ $ do
    meta_ [charset_ "UTF-8"]
    title_ "Panel Admin"
    style_ "body{background:#111;color:white;font-family:sans-serif;} \
           \a{color:lightgreen;} \
           \table{width:100%;border-collapse:collapse;margin-top:20px;} \
           \th,td{border:1px solid #333;padding:8px;}"
  body_ $ do
    h1_ "🛠️ Admin"
    a_ [href_ "/"] "Inicio"
    " | "
    a_ [href_ "/admin/nuevo"] "➕ Nuevo usuario"

    table_ $ do
      tr_ $ do
        th_ "ID"
        th_ "Nombre"
        th_ "Email"
        th_ "Rol"
        th_ "Acciones"

      mapM_ renderUser usuarios


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


------------------------------------------------------------
-- 📝 FORMULARIO USUARIO (para Admin)
------------------------------------------------------------
formUsuario :: String -> Maybe Usuario -> Html ()
formUsuario actionPath maybeU = html_ $ do
  head_ $ do
    meta_ [charset_ "UTF-8"]
    title_ "Formulario Usuario"
    style_ "body { background:#111; color:white; font-family:sans-serif; } \
           \input, select { margin:4px; padding:6px; border-radius:6px; } \
           \a { color: lightgreen; }"
  body_ $ do
    h1_ (if maybeU == Nothing then "Crear usuario" else "Editar usuario")

    form_ [method_ "post", action_ (T.pack actionPath)] $ do
      p_ $ do
        "Nombre: "
        input_ [type_ "text", name_ "nombre",
                value_ (maybe "" (T.pack . nombre) maybeU)]

      p_ $ do
        "Email: "
        input_ [type_ "email", name_ "email",
                value_ (maybe "" (T.pack . email) maybeU)]

      p_ $ do
        "Contraseña: "
        input_ [type_ "password", name_ "password"]

      p_ $ do
        "Rol: "
        select_ [name_ "rol"] $ do
          option_ [value_ "User",
                   selected_ (if maybeU /= Nothing && rol (maybeUsr maybeU) == User then "selected" else "")]
                   "User"
          option_ [value_ "Admin",
                   selected_ (if maybeU /= Nothing && rol (maybeUsr maybeU) == Admin then "selected" else "")]
                   "Admin"

      p_ $ input_ [type_ "submit", value_ "Guardar"]

      p_ $
        a_ [href_ "/admin"] "⬅️ Volver"

  where
    maybeUsr (Just u) = u
    maybeUsr _        = error "formUsuario: impossible"


------------------------------------------------------------
-- 🗺️ PÁGINA GENERAL DE RUTAS
------------------------------------------------------------
paginaRutas :: Html ()
paginaRutas = html_ $ do
  head_ $ do
    meta_ [charset_ "UTF-8"]
    title_ "Rutas"
    style_ "body{background:#111;color:white;font-family:sans-serif;text-align:center;} \
           \a{color:lightgreen;margin:10px;}"
  body_ $ do
    h1_ "🗺️ Rutas"
    a_ [href_ "/"] "🏠 Inicio"
    a_ [href_ "/rutas/iniciar"] "▶️ Iniciar ruta"
    a_ [href_ "/rutas/historial"] "📜 Historial"


------------------------------------------------------------
-- 🟢 INICIAR RUTA — JS COMPLETO (Leaflet + pausa/reanudar + envío JSON)
------------------------------------------------------------
paginaIniciarRuta :: Html ()
paginaIniciarRuta = html_ $ do
  head_ $ do
    meta_ [charset_ "UTF-8"]
    title_ "Iniciar ruta"

    -- Leaflet
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

    button_ [id_ "pauseBtn"] "⏸️ Pausar"
    button_ [id_ "stopBtn"]  "⏹️ Terminar ruta"
    button_ [id_ "backBtn"]  "⬅️ Volver"

    script_ inicioRutaJS


------------------------------------------------------------
-- 📜 HISTORIAL — LISTA DE BOTONES
------------------------------------------------------------
paginaHistorial :: [Ruta] -> Html ()
paginaHistorial rutas = html_ $ do
  head_ $ do
    meta_ [charset_ "UTF-8"]
    title_ "Historial de rutas"
    style_
      "body { background:#111; color:white; font-family:sans-serif; text-align:center; }\
      \a{color:lightgreen;margin:8px;}\
      \.ruta-btn{display:block;background:#222;border:1px solid #444;padding:10px;margin:10px auto;width:300px;border-radius:10px;}"

  body_ $ do
    h1_ "📜 Historial de rutas"
    a_ [href_ "/rutas"] "⬅️ Volver"

    if null rutas
      then p_ "Todavía no tienes rutas guardadas."
      else mapM_ renderRutaBoton rutas


renderRutaBoton :: Ruta -> Html ()
renderRutaBoton r = a_
  [ class_ "ruta-btn"
  , href_ ("/rutas/ver/" <> T.pack (show (rutaId r)))
  ] $ do
      strong_ $ toHtml ("Ruta #" ++ show (rutaId r))
      br_ []
      toHtml ("Distancia: " ++ show (round (rutaDistancia r)) ++ " m")
      br_ []
      toHtml ("Velocidad media: " ++ show (realToFrac (rutaVelMedia r) :: Double) ++ " km/h")
    
------------------------------------------------------------
-- 🔧 3D — CUBO PARA LA PÁGINA DE VEHÍCULO
------------------------------------------------------------
escena3D :: T.Text
escena3D = T.pack $ unlines
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
  , "light.position.set(5,5,5);"
  , "scene.add(light);"
  , "camera.position.z = 2;"
  , "function animate(){ requestAnimationFrame(animate); cube.rotation.x+=0.01; cube.rotation.y+=0.01; renderer.render(scene,camera);} animate();"
  ]


inicioRutaJS :: T.Text
inicioRutaJS = T.pack $ unlines
  [
    "let map = L.map('map').setView([0,0], 13);"
  , "L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',{maxZoom:19}).addTo(map);"
  , "let track = L.polyline([], {color:'lime'}).addTo(map);"

  , "let totalDist = 0;"
  , "let lastPos = null;"
  , "let lastTime = null;"
  , "let startTime = Date.now();"
  , "let gotRealGPS = false;"
  , "let paused = false;"
  , "let pauseStart = null;"

  , "function updatePos(pos) {"
  , "  if (paused) return;"
  , "  let now = Date.now();"
  , "  let lat = pos.coords.latitude;"
  , "  let lon = pos.coords.longitude;"

  , "  if (!lastPos) {"
  , "    lastPos = {lat, lon};"
  , "    lastTime = now;"
  , "    map.setView([lat,lon], 16);"
  , "    track.addLatLng([lat,lon]);"
  , "    return;"
  , "  }"

  , "  let d = map.distance([lat,lon], [lastPos.lat,lastPos.lon]);"
  , "  let dt = (now - lastTime) / 1000;"

  , "  if (d > 0.5) {"
  , "    totalDist += d;"
  , "    track.addLatLng([lat, lon]);"
  , "  }"

  , "  let v = dt > 0 ? (d / dt) * 3.6 : 0;"
  , "  lastPos = {lat, lon};"
  , "  lastTime = now;"

  , "  let totalTime = (now - startTime) / 1000;"
  , "  document.getElementById('stats').innerText ="
  , "    `Distancia: ${totalDist.toFixed(1)} m | Velocidad: ${v.toFixed(1)} km/h | Tiempo: ${totalTime.toFixed(1)} s`;"
  , "}"

  , "document.getElementById('pauseBtn').onclick = function() {"
  , "  if (!paused) {"
  , "    paused = true;"
  , "    pauseStart = Date.now();"
  , "    this.innerText = '▶️ Reanudar';"
  , "  } else {"
  , "    paused = false;"
  , "    if (pauseStart !== null) {"
  , "      const pausedDuration = Date.now() - pauseStart;"
  , "      startTime += pausedDuration;"
  , "      if (lastTime !== null) lastTime += pausedDuration;"
  , "    }"
  , "    pauseStart = null;"
  , "    this.innerText = '⏸️ Pausar';"
  , "  }"
  , "};"

  , "document.getElementById('backBtn').onclick = () => { window.location.href = '/rutas'; };"

  , "document.getElementById('stopBtn').onclick = async function () {"
  , "  paused = true;"
  , "  let endTime = Date.now();"
  , "  let duration = (endTime - startTime) / 1000;"
  , "  let avgSpeed = duration > 0 ? (totalDist / duration) * 3.6 : 0;"

  , "  const payload = {"
  , "    distancia: totalDist,"
  , "    duracion: duration,"
  , "    velocidad: avgSpeed,"
  , "    puntos: track.getLatLngs().map(p => [p.lat, p.lng]),"
  , "    paradas: []"
  , "  };"

  , "  const resp = await fetch('/rutas/terminar', {"
  , "    method: 'POST',"
  , "    headers: {'Content-Type': 'application/json'},"
  , "    body: JSON.stringify(payload)"
  , "  });"

  , "  if (resp.ok) {"
  , "    alert('Ruta guardada con éxito');"
  , "    window.location.href = '/rutas/historial';"
  , "  } else {"
  , "    alert('❌ Error guardando ruta');"
  , "  }"
  , "};"

  , "navigator.geolocation.watchPosition(p => {"
  , "  gotRealGPS = true;"
  , "  updatePos(p);"
  , "}, err => console.warn('⚠️ Error GPS real:', err));"

  , "setTimeout(() => {"
  , "  if (gotRealGPS) return;"

  , "  window.geoSim = { callbacks: [updatePos] };"
  , "  navigator.geolocation.watchPosition = function(cb) {"
  , "    window.geoSim.callbacks.push(cb);"
  , "  };"

  , "  let path = [];"
  , "  let lat = 40.416800;"
  , "  let lon = -3.703800;"
  , "  for (let j = 0; j < 500; j++) { lat += 0.00005; path.push([lat, lon]); }"

  , "  let idx = 0;"
  , "  setInterval(() => {"
  , "    if (paused) return;"
  , "    const pos = { coords: { latitude: path[idx][0], longitude: path[idx][1], speed: null }};"
  , "    window.geoSim.callbacks.forEach(cb => cb(pos));"
  , "    idx = (idx + 1) % path.length;"
  , "  }, 2000);"

  , "}, 5000);"
  ]