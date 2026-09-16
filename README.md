# Open Modular Statistical Calculator

A modular Shiny web application for descriptive statistics. Load a dataset in the
browser, explore it with interactive tables, run statistical operations, and build
graphs — everything through a plugin-style architecture that is easy to extend.

## Features

- **Data upload** — load CSV files and preview them in an interactive, sortable table (DT).
- **Statistical operations** — mean, median, mode, quantiles, variance, standard deviation,
  skewness, kurtosis, IQR, coefficient of variation, and more.
- **Two-variable analysis** — correlation, two-way frequency tables (absolute, relative,
  with margins), plus conditional and marginal statistics (means, medians, modes,
  quantiles, variances, standard deviations).
- **Graphs** — histogram, boxplot, density plot, bar chart, Pareto chart, pie chart,
  scatter plot, stem-and-leaf plot, cumulative sum, and two-way barplot, with
  configurable colors and parameters.
- **Multilingual interface** — English, Spanish, and Italian, powered by `shiny.i18n`.
  New languages are added by dropping a folder into `translations/` — no code changes.
- **Column and row tools** — recast column types, and operate on rows or columns of the
  loaded dataset.

## Extensible architecture

The app discovers its own components at startup, so extending it means adding a file,
not editing the core:

| Folder | What it holds | How it is registered |
|---|---|---|
| `modules/` | Shiny modules (`mod_<id>.R` with `mod_<id>_ui` / `mod_<id>_server`) | Auto-discovered and sorted by `mod_<id>_order` |
| `operations/<family>/` | Statistical operations (univariant, bivariant, conditional, marginal) | Operation registry |
| `graphs/` | Graph definitions | Graph registry |
| `translations/<lang>/` | CSV translation tables | Discovered per language folder |

## Getting started

### Requirements

- R (4.x or later)

### Run the app

From RStudio, open the project and press **Run App** on `app.R`, or from a terminal:

```bash
Rscript.exe app.R
```

Missing packages (`shiny`, `bslib`, `DT`, `ggplot2`, `qcc`, `e1071`, `shiny.i18n`,
`colourpicker`) are installed automatically on first launch. The app opens in your
browser at **http://localhost:6357**.

### Launch options

| Flag | Effect |
|---|---|
| `--port=<n>` | Serve on a custom port (fails fast if the port is taken) |
| `--server_mode` | Listen on `0.0.0.0` so other machines on the local network can connect |

```bash
Rscript.exe app.R --port=8080 --server_mode
```

## Project layout

```
app.R              # Launcher: port/host resolution, package install, startup
core/              # Registries, i18n, data store, UI and server
modules/           # UI modules: upload, table, operations, graphs, column/row tools
operations/        # Statistical operations by family
graphs/            # Graph definitions
translations/      # Language folders with CSV translation tables
exampleData/       # Sample dataset (encuesta.csv)
```

## Example data

A sample survey dataset is included in `exampleData/encuesta.csv` so you can try the
app right away.
