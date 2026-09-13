# This file defines the graph registry.
# Each file in graphs/ registers one graph type here.
.graph_registry <- new.env(parent = emptyenv())
.graph_registry$graphs <- list()

# This function registers one graph.
# It checks the function and the parameter specs.
register_graph <- function(name,
                           fn,
                           types,
                           min_cols = 1,
                           max_cols = 1,
                           
                           render = "plot",
                           params = list(),
                           colours = FALSE,
                           
                           regression_toggle = FALSE) {
  stopifnot(
    is.function(fn),
    length(formals(fn)) >= 3,
    
    is.logical(colours),
    length(colours) == 1,
    
    is.logical(regression_toggle),
    length(regression_toggle) == 1
  )
  
  invalid_specs <- vapply(params, function(spec) {
    !is.list(spec) ||
      !all(c("id", "type", "label", "default") %in% names(spec))
    
  }, logical(1))
  
  if (any(invalid_specs)) {
    stop(
      "Graph '",
      name,
      "': every params entry must be created with ",
      "p_num()/p_colour()/p_text()."
    )
  }
  .graph_registry$graphs[[name]] <- list(
    fn = fn,
    types = types,
    min_cols = min_cols,
    max_cols = max_cols,
    
    render = render,
    params = params,
    colours = colours,
    
    regression_toggle = regression_toggle
  )
}
# This block loads every graph file from the graphs folder.
local({
  graph_dir <- app_file("graphs")
  
  if (!dir.exists(graph_dir)) {
    warning("Graphs folder not found; no graphs loaded.")
    
    return(invisible())
    
  }
  
  for (graph_file in list.files(
    graph_dir,
    pattern = "\\.R$",
    full.names = TRUE,
    recursive = TRUE
  )) {
    source(graph_file, local = new.env(parent = environment()))
    
  }
  
})

# This variable holds all registered graphs.
available_graphs <- .graph_registry$graphs

# This function returns the graphs that fit the selected columns.
# A graph fits when the column count and types match its limits.
applicable_graphs <- function(selected_types) {
  n_selected <- length(selected_types)
  
  if (n_selected == 0)
    return(character(0))
  
  fits_selection <- vapply(available_graphs, function(graph) {
    n_selected >= graph$min_cols &&
      n_selected <= graph$max_cols &&
      all(selected_types %in% graph$types)
    
  }, logical(1))
  names(available_graphs)[fits_selection]
  
}

# This function runs one graph and returns its output.
run_graph <- function(df, graph_name, cols, params = list()) {
  # Stop with a clear message when the name is not in the registry.
  if (!graph_name %in% names(available_graphs)) {
    stop("Unknown graph: ", graph_name)
  }
  graph <- available_graphs[[graph_name]]
  
  graph$fn(df, cols, params)
  
}
