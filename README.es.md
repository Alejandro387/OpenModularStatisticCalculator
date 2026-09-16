# Calculadora Estadística / Open Modular Statistical Calculator

Aplicación web modular en Shiny para estadística descriptiva. Carga un conjunto de
datos en el navegador, explóralo con tablas interactivas, ejecuta operaciones
estadísticas y construye gráficos — todo mediante una arquitectura de estilo plugin
fácil de extender.

## Funcionalidades

- **Carga de datos** — carga archivos CSV y previsualízalos en una tabla interactiva
  y ordenable (DT).
- **Operaciones estadísticas** — media, mediana, moda, cuantiles, varianza, desviación
  típica, asimetría, curtosis, rango intercuartílico, coeficiente de variación y más.
- **Análisis de dos variables** — correlación, tablas de contingencia (absolutas,
  relativas, con márgenes), además de estadísticos condicionales y marginales
  (medias, medianas, modas, cuantiles, varianzas y desviaciones típicas).
- **Gráficos** — histograma, diagrama de caja, gráfico de densidad, gráfico de barras,
  diagrama de Pareto, gráfico circular, diagrama de dispersión, diagrama de tallo y
  hojas, suma acumulada y gráfico de barras de dos vías, con colores y parámetros
  configurables.
- **Interfaz multiidioma** — inglés, español e italiano, mediante `shiny.i18n`.
  Los idiomas nuevos se añaden dejando una carpeta en `translations/` — sin cambiar
  código.
- **Herramientas de columnas y filas** — reconvertir tipos de columna y operar sobre
  filas o columnas del conjunto de datos cargado.

## Arquitectura extensible

La aplicación descubre sus propios componentes al arrancar, así que extenderla
significa añadir un archivo, no editar el núcleo:

| Carpeta | Contenido | Cómo se registra |
|---|---|---|
| `modules/` | Módulos de Shiny (`mod_<id>.R` con `mod_<id>_ui` / `mod_<id>_server`) | Descubrimiento automático, ordenados por `mod_<id>_order` |
| `operations/<familia>/` | Operaciones estadísticas (univariante, bivariante, condicional, marginal) | Registro de operaciones |
| `graphs/` | Definiciones de gráficos | Registro de gráficos |
| `translations/<idioma>/` | Tablas de traducción en CSV | Descubrimiento por carpeta de idioma |

## Primeros pasos

### Requisitos

- R (4.x o superior)

### Ejecutar la aplicación

Desde RStudio, abre el proyecto y pulsa **Run App** en `app.R`, o desde una terminal:

```bash
Rscript.exe app.R
```

Los paquetes que falten (`shiny`, `bslib`, `DT`, `ggplot2`, `qcc`, `e1071`,
`shiny.i18n`, `colourpicker`) se instalan automáticamente en el primer arranque.
La aplicación se abre en el navegador en **http://localhost:6357**.

### Opciones de arranque

| Parámetro | Efecto |
|---|---|
| `--port=<n>` | Servir en un puerto personalizado (falla rápido si el puerto está ocupado) |
| `--server_mode` | Escucha en `0.0.0.0` para que otras máquinas de la red local puedan conectarse |

```bash
Rscript.exe app.R --port=8080 --server_mode
```

## Estructura del proyecto

```
app.R              # Arranque: resolución de puerto/host, instalación de paquetes, inicio
core/              # Registros, i18n, almacén de datos, UI y servidor
modules/           # Módulos de UI: carga, tabla, operaciones, gráficos, herramientas de columnas/filas
operations/        # Operaciones estadísticas por familia
graphs/            # Definiciones de gráficos
translations/      # Carpetas de idioma con tablas de traducción en CSV
exampleData/       # Conjunto de datos de ejemplo (encuesta.csv)
```

## Datos de ejemplo

Se incluye un conjunto de datos de una encuesta en `exampleData/encuesta.csv` para
que puedas probar la aplicación de inmediato.
