# =====================================================================
# Calculadora Estadística — launcher
#
# Order of operations:
#   1. Resolve launch settings from the command line (fail fast)
#   2. Install and load required packages
#   3. Build the app environment and source all app code into it
#   4. Start Shiny
# =====================================================================

# ---- 1. Launch settings ---------------------------------------------
# The default port and host are used by every launch; an Rscript.exe launch
# can override them with --port= and --server_mode. A wrong or already-taken
# port stops the app with an error before anything else loads.

DEFAULT_PORT <- 6357

# A server that is listening accepts a connection, so a successful connection
# means the port is taken. (On Windows, httpuv, which shiny uses, can bind a
# port over another server, so testing the bind alone is not enough.)
port_is_listening <- function(port) {
  tryCatch({
    con <- socketConnection("127.0.0.1", port, open = "r", timeout = 1)
    close(con)
    TRUE
  }, error = function(e) FALSE)
}

# A port is bindable when we can create a listening socket on it.
port_binds <- function(port) {
  socket <- tryCatch(serverSocket(port), error = function(e) NULL)
  if (is.null(socket)) return(FALSE)
  close(socket)
  TRUE
}

port_is_free <- function(port) {
  !port_is_listening(port) && port_binds(port)
}

# --server_mode serves every machine of the local network; without it, only
# this machine.
parse_launch_host <- function() {
  if (any(commandArgs() == "--server_mode")) "0.0.0.0" else "127.0.0.1"
}

parse_launch_port <- function(default_port) {
  port_args <- grep("^--port=", commandArgs(), value = TRUE)
  if (!length(port_args)) return(default_port)
  port <- suppressWarnings(as.integer(sub("^--port=", "", port_args[1])))
  if (is.na(port) || port < 1 || port > 65535) {
    stop("Invalid --port parameter: '", port_args[1],
         "'. Use a port number between 1 and 65535.")
  }
  if (!port_is_free(port)) {
    stop("Port ", port, " is not available; it may already be in use.")
  }
  port
}

# Resolved before anything else loads, so a wrong parameter fails immediately.
launch_settings <- list(
  port = parse_launch_port(DEFAULT_PORT),
  host = parse_launch_host()
)

# Pins the default port for every way of launching the app.
options(shiny.port = DEFAULT_PORT)


# --Packages--
REQUIRED_PACKAGES <- c("shiny", "bslib", "DT", "ggplot2", "qcc",
                       "e1071", "shiny.i18n", "colourpicker", "datasets")

install_missing_packages <- function(packages) {
  missing <- packages[!vapply(packages,
    requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, dependencies = TRUE)
  }
}

load_packages <- function(packages) {
  invisible(lapply(packages, library, character.only = TRUE))
}

install_missing_packages(REQUIRED_PACKAGES)
load_packages(REQUIRED_PACKAGES)


# --App root--
# The root folder is the folder that holds app.R, core/, and modules/.
# Rscript exposes the script path via --file=; RStudio's Run App (and
# shiny::runApp()) run with the working directory set to the app folder.
# If neither yields a folder with the expected layout, stop with a clear
# error instead of guessing.
find_app_root <- function() {
  root_from_rscript <- function() {
    file_args <- grep("^--file=", commandArgs(), value = TRUE)
    if (length(file_args)) dirname(sub("^--file=", "", file_args[1])) else character(0)
  }

  candidate <- normalizePath(c(root_from_rscript(), getwd())[1],
                             winslash = "/", mustWork = FALSE)

  if (!dir.exists(file.path(candidate, "core")) ||
      !dir.exists(file.path(candidate, "modules"))) {
    stop("App root not found: '", candidate, "' does not contain core/ ",
         "and modules/. Launch with RStudio's Run App, or with ",
         "Rscript.exe app.R from any directory.")
  }
  candidate
}

app_root <- find_app_root()
app_file <- function(...) file.path(app_root, ...)

# --App environment--
# The app environment holds all app code, keeping the global workspace clean.
build_app_env <- function() {
  env <- new.env(parent = globalenv())

  # The app pins the modal functions of the shiny package here, because a
  # same-named object in the global workspace would otherwise replace them.
  env$showModal <- shiny::showModal
  env$removeModal <- shiny::removeModal
  env$showNotification <- shiny::showNotification

  env$app_file <- function(...) file.path(app_root, ...)

  # Core files load in dependency order: the helper files first, the UI and
  # the server last.
  core_files <- c(
    "core/i18n.R",
    "core/data_store.R",
    "core/operation_registry.R",
    "core/graph_params.R",
    "core/graph_registry.R",
    "core/module_registry.R",
    "core/ui_helpers.R",
    "core/ui.R",
    "core/server.R"
  )
  for (file in core_files) {
    source(app_file(file), local = env)
  }

  env
}

app_env <- build_app_env()

# --Start Shiny--
# Under RStudio (Run App), the file returns the app object and the launcher
# manages the app. Under Rscript.exe, the app is served directly on the port
# and host resolved in step 1, and a failed port stops the launch with an
# error.
app <- shinyApp(app_env$ui, app_env$server)
if (any(grepl("^--file=", commandArgs()))) {
  runApp(app, port = launch_settings$port, host = launch_settings$host)
} else {
  app
}
