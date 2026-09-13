# This file defines the module registry.
# It finds the module files, reads them, and sorts the modules.

# This cache holds the discovered modules.
.module_cache <- new.env(parent = emptyenv())

# This function finds and loads all modules.
# It reads each file into its own environment.
discover_modules <- function(dir = NULL, refresh = FALSE) {
  if (!refresh &&
      !is.null(.module_cache$modules))
    return(.module_cache$modules)
  
  if (is.null(dir))
    dir <- app_file("modules")
  if (!dir.exists(dir))
    stop("Modules folder not found; no modules loaded.")
  
  # The module environments inherit from this environment.
  app_ns <- environment()
  
  # The pattern matches the subfolder-relative path, so it accepts a
  # subfolder prefix before the mod_ filename.
  files <- list.files(dir, pattern = "(^|[/\\])mod_.*\\.R$", recursive = TRUE)  # alphabetical
  mods <- lapply(files, function(mod_file) {
    id <- sub("^mod_(.*)\\.R$", "\\1", basename(mod_file))
    mod_env <- new.env(parent = app_ns)
    source(file.path(dir, mod_file), local = mod_env)
    ui <- get0(paste0("mod_", id, "_ui"),
               envir = mod_env,
               inherits = FALSE)
    server <- get0(paste0("mod_", id, "_server"),
                   envir = mod_env,
                   inherits = FALSE)
    # The registry skips a file that lacks one of the two functions.
    if (is.null(ui) || is.null(server)) {
      warning("Skipping '",
              mod_file,
              "': missing mod_",
              id,
              "_ui or mod_",
              id,
              "_server.")
      return(NULL)
    }
    # This list holds one module: the id, the functions, the place, and the sort order.
    list(
      id = id,
      ui = ui,
      server = server,
      where = rlang::`%||%`(get0(
        paste0("mod_", id, "_where"),
        envir = mod_env,
        inherits = FALSE
      ), "sidebar"),
      order = rlang::`%||%`(get0(
        paste0("mod_", id, "_order"),
        envir = mod_env,
        inherits = FALSE
      ), Inf)
    )
  })
  # This line removes the skipped modules.
  mods <- Filter(Negate(is.null), mods)
  
  # This block sorts the modules by order value, and then by id.
  orders <- vapply(mods, function(module)
    module$order, numeric(1))
  ids <- vapply(mods, function(module)
    module$id, character(1))
  mods <- mods[order(orders, ids)]
  
  # This line saves the modules in the cache.
  .module_cache$modules <- mods
  mods
}

# This function builds the UI of the modules in one area.
build_module_ui <- function(where) {
  mods <- Filter(function(module)
    module$where == where, discover_modules())
  unclass(do.call(tagList, lapply(mods, function(module)
    module$ui(module$id))))
}

# This function starts the server of every module.
start_module_servers <- function(store) {
  for (module in discover_modules())
    module$server(module$id, store)
  invisible(NULL)
}
