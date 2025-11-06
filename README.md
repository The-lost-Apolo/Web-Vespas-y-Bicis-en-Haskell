# 🏍️ Mi Garaje Vespa

**Tu garaje digital personal para motos y bicis.**  
Registra tus vehículos, visualiza estadísticas, guarda tus rutas y comparte experiencias con otros apasionados del motor.

---

## 💡 Qué es Mi Garaje Vespa

**Mi Garaje Vespa** es una aplicación web ligera que permite a los usuarios gestionar sus motocicletas y bicicletas desde un entorno visual, accesible y moderno.  

Pensada inicialmente como un proyecto personal, la idea evolucionó hacia una plataforma donde los aficionados pueden **organizar su garaje**, **registrar rutas GPS**, y **conectarse con comunidades locales** de Vespas y ciclistas.

---

## ✨ Características principales

- 📋 Registro de vehículos (modelo, año, revisiones, kilometraje).  
- 🖼️ Galería de fotos de cada moto o bici.  
- 📊 Estadísticas de uso: kilómetros totales, revisiones pendientes.  
- 🧭 Visualización 3D interactiva de los vehículos (Three.js).  
- 🗺️ Sistema de rutas GPS con mapa interactivo.  
- 🏍️ Gestión de garajes personales y rutas cercanas.  
- 🧑‍🤝‍🧑 Comunidades de usuarios (asociaciones de bicis o Vespas).  
- 🎨 Interfaz minimalista con modo oscuro y diseño moderno.

---

## 🧱 Tecnologías utilizadas

**Backend:** [Haskell](https://www.haskell.org/) — *Servant, Lucid, Warp*  
**Frontend:** HTML generado con *Lucid* + *TailwindCSS*  
**Base de datos:** SQLite  
**Mapas:** [Leaflet.js](https://leafletjs.com/)  
**Render 3D:** [Three.js](https://threejs.org/)  

---

## ⚙️ Instalación rápida

### Prerrequisitos

- [Stack](https://docs.haskellstack.org/en/stable/README/)  
- [SQLite](https://www.sqlite.org/download.html)

### Pasos

```bash
# Clonar el repositorio
git clone https://github.com/miusuario/mi-garaje-vespa.git
cd mi-garaje-vespa

# Instalar dependencias y ejecutar
stack setup
stack build
stack run
```

La aplicación se abrirá en [http://localhost:8080](http://localhost:8080).

---

## 🖼️ Capturas de ejemplo

*(En desarrollo — próximamente se añadirán imágenes y demo en vídeo)*

---

## 🧭 Roadmap

| Etapa | Objetivo | Estado |
|-------|-----------|--------|
| ✅ **MVP** | Registro y visualización de vehículos, fotos, kilometraje | Completado |
| 🚧 **Rutas GPS** | Sistema para iniciar rutas, registrar posición, velocidad y tiempo real | En desarrollo |
| 🔜 **Multimedia en mapa** | Permitir añadir fotos y vídeos sobre el mapa interactivo de la ruta | Próximamente |
| 🧑‍🤝‍🧑 **Asociaciones** | Crear comunidades de bicis y Vespas, organizar rutas conjuntas | Pendiente |
| 🎨 **Diseño y estilos** | Añadir CSS moderno (Tailwind o custom) y modo oscuro | Pendiente |
| 🌍 **API REST pública** | Endpoints para integración con apps móviles | Planificado |
| 📱 **App móvil** | Cliente en Flutter o React Native conectado al backend Haskell | Planificado |

---

> _“Cada kilómetro cuenta. Haz que cada ruta tenga memoria.”_
