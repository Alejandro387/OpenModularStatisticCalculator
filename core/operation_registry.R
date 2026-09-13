# This file defines the operation registry.
# Each file in operations/ registers one statistical operation here.
.op_registry <- new.env(parent = emptyenv())
.op_registry$ops <- list()

# This helper returns the most frequent value.
# It ignores the missing values.
stat_mode <- function(values) {
  values <- values[!is.na(values)]
  if (length(values) == 0) return(NA)
  unique_values <- unique(values)
  unique_values[which.max(tabulate(match(values, unique_values)))]
}

# This helper builds the margin input for the two-way tables.
margin_param_ui <- function(ns) {
  selectInput(
    ns("margin_n"), i18n$t("Margin (1 or 2)"),
    choices = c("1", "2"), selected = "1"
  )
}
# This helper reads the margin input.
# It uses margin 1 when the input is not valid.
parse_margin_param <- function(input) {
  margin <- suppressWarnings(as.integer(input$margin_n))
  if (is.na(margin) || !margin %in% c(1, 2)) margin <- 1L
  list(n = margin)
}

# This helper checks the types for the conditional operations.
# The first column is numeric, and the second column holds the groups.
conditional_types_check <- function(types) {
  length(types) == 2 &&
    types[1] %in% c("discrete", "continuous") &&
    types[2] %in% c("nominal", "discrete")
}

# This function registers one operation.
# It checks the flags and saves the entry.
register_operation <- function(name, fn, types, min_cols = 1, max_cols = 1,
                                takes_params = FALSE,
                                param_ui = NULL, parse_param = NULL) {
  stopifnot(is.logical(takes_params), length(takes_params) == 1)
  if (!isTRUE(takes_params) && !is.null(parse_param)) {
    stop("Operation '", name, "': parse_param requires takes_params = TRUE.")
  }
  .op_registry$ops[[name]] <- list(
    fn = fn, types = types, min_cols = min_cols, max_cols = max_cols,
    takes_params = takes_params,
    param_ui = param_ui, parse_param = parse_param
  )
}

# This block loads every operation file from the operations folder.
local({
  op_dir <- app_file("operations")
  if (!dir.exists(op_dir)) {
    warning("Operations folder not found; no statistical operations loaded.")
    return(invisible())
  }
  for (op_file in list.files(op_dir, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)) {
    source(op_file, local = new.env(parent = environment()))
  }
})

# This variable holds all registered operations.
stat_operations <- .op_registry$ops

# This function returns the operations that fit the selected columns.
# Some operations check the types with a function instead of a simple list.
applicable_operations <- function(selected_types) {
  n_selected <- length(selected_types)
  if (n_selected == 0) return(character(0))
  fits_selection <- vapply(stat_operations, function(operation) {
    types_ok <- if (is.function(operation$types)) {
      isTRUE(operation$types(selected_types))
    } else {
      all(selected_types %in% operation$types)
    }
    n_selected >= operation$min_cols && n_selected <= operation$max_cols && types_ok
  }, logical(1))
  names(stat_operations)[fits_selection]
}

# This function runs one operation and returns its result.
run_operation <- function(df, op_name, cols, params = list()) {
  # Stop with a clear message when the name is not in the registry.
  if (!op_name %in% names(stat_operations)) {
    stop("Unknown operation: ", op_name)
  }
  operation <- stat_operations[[op_name]]
  if (isTRUE(operation$takes_params)) {
    operation$fn(df, cols, params)
  } else {
    operation$fn(df, cols)
  }
}
