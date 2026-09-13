# This file starts the app.
# It loads the packages, finds the app folder, and builds the app environment.
# Then it loads all code files in order and starts Shiny.

# This block chooses the port and the host of the app.
# The default port and host are used by every launch; an Rscript.exe launch
# can override them with the --port= and --server_mode parameters. A wrong or
# already-taken port stops the app with an error before anything else loads.
default_port <- 6357

# This line pins the default port for every way of launching the app.
options(shiny.port = default_port)

# This helper checks that nothing is serving on a port already.
# A server that is listening accepts a connection, so a successful
# connection means the port is taken. (On Windows, httpuv, which shiny
# uses, can bind a port over another server, so testing the bind is
# not enough.)
port_is_free <- function(port) {
  taken <- tryCatch({
    con <- socketConnection("127.0.0.1", port, open = "r", timeout = 1)
    close(con)
    TRUE
  }, error = function(e) FALSE)
  if (taken) return(FALSE)
  socket <- tryCatch(serverSocket(port), error = function(e) NULL)
  if (is.null(socket)) return(FALSE)
  close(socket)
  TRUE
}

# This helper reads the --server_mode parameter of the command line,
# Without it, the default serves only this machine, and 
# --server_mode serves every machine of the local network.
command_line_host <- function() {
  if (any(commandArgs() == "--server_mode")) return("0.0.0.0")
  "127.0.0.1"
}

# This helper reads the --port= parameter of the command line, 
# and stops when the port is invalid or not available.
command_line_port <- function() {
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

# These lines resolve the port and the host before anything else is
# loaded, so a wrong parameter stops the app immediately.
launch_port <- command_line_port()
launch_host <- command_line_host()

# This block installs and loads the packages that the app needs.
required_packages <- c("shiny", "bslib", "DT", "ggplot2", "qcc", "e1071", "shiny.i18n", "colourpicker")
for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) install.packages(package)
  library(package, character.only = TRUE)
}

# This block finds the app root folder.
# The root folder is the folder that holds core/ and modules/.
app_root <- local({
  # This helper checks that a folder holds core/ and modules/.
  has_app_layout <- function(folder) {
    dir.exists(file.path(folder, "core")) && dir.exists(file.path(folder, "modules"))
  }

  # R records the file of each sourced file.
  # This helper collects the folders of those files.
  from_source_frames <- function() {
    source_dirs <- Filter(is.character, lapply(rev(sys.frames()), function(frame) {
      tryCatch(get("ofile", envir = frame, inherits = FALSE), error = function(e) NULL)
    }))
    if (length(source_dirs)) dirname(unlist(source_dirs, use.names = FALSE)) else character(0)
  }

  # This helper reads the folder from the --file= argument of Rscript.
  from_rscript_arg <- function() {
    file_args <- grep("^--file=", commandArgs(), value = TRUE)
    if (length(file_args)) dirname(sub("^--file=", "", file_args)) else character(0)
  }

  # This helper collects the working directory and its parent folders.
  from_wd_ancestors <- function() {
    folder <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    ancestors <- folder
    for (i in 1:10) {
      folder <- dirname(folder)
      ancestors <- c(ancestors, folder)
      if (identical(folder, dirname(folder))) break  # reached filesystem root
    }
    ancestors
  }

  # The code uses the first candidate folder that has the app layout.
  candidates <- unique(c(from_source_frames(), from_rscript_arg(), from_wd_ancestors()))
  found <- Filter(has_app_layout, candidates)
  if (!length(found)) {
    stop("App root not found: no candidate directory contains both core/ ",
         "and modules/. Launch the app from inside the project, or source ",
         "app.R directly.")
  }
  normalizePath(found[1], winslash = "/", mustWork = TRUE)
})

# The app environment holds all app code.
# It keeps the global workspace clean.
app_env <- new.env(parent = globalenv())

# The app pins the modal functions of the shiny package here.
# A same-named object in the global workspace would otherwise replace them.
app_env$showModal <- shiny::showModal
app_env$removeModal <- shiny::removeModal
app_env$showNotification <- shiny::showNotification

# This helper builds a path from the app root.
app_env$app_file <- function(...) file.path(app_root, ...)
app_file <- app_env$app_file

# This block loads the core files.
# The helper files load first, and the UI and the server load last.
source(app_file("core", "i18n.R"), local = app_env)
source(app_file("core", "data_store.R"), local = app_env)
source(app_file("core", "operation_registry.R"), local = app_env)
source(app_file("core", "graph_params.R"), local = app_env)
source(app_file("core", "graph_registry.R"), local = app_env)
source(app_file("core", "module_registry.R"), local = app_env)
source(app_file("core", "ui_helpers.R"), local = app_env)

source(app_file("core", "ui.R"), local = app_env)
source(app_file("core", "server.R"), local = app_env)

# This block starts the Shiny app.
# Under RStudio (Run App), the file returns the app object and the launcher
# manages the app. Under Rscript.exe, the app is served directly on the
# port and host chosen at the top, and a failed port stops the launch with
# an error.
app <- shinyApp(app_env$ui, app_env$server)
if (any(grepl("^--file=", commandArgs()))) {
  runApp(app, port = launch_port, host = launch_host)
} else {
  app
}
