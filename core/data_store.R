# This file defines the data store for one session.
# The store holds the data frame, the column types, and the selected columns.

# This function infers the measurement type of one column.
# A numeric column with few distinct values is discrete. A text column is nominal.
infer_col_type <- function(column, discrete_threshold = 10) {
  if (is.numeric(column)) {
    n_distinct <- length(unique(column[!is.na(column)]))
    if (is.integer(column) ||
        n_distinct <= discrete_threshold)
      "discrete"
    else
      "continuous"
  } else {
    "nominal"
  }
}

# This function applies infer_col_type to every column.
infer_col_types <- function(df) {
  vapply(df, infer_col_type, character(1))
}

# This helper returns a reactive with the types of the selected columns.
store_selected_types <- function(store) {
  reactive({
    cols <- store$selected_cols()
    unname(store$col_types()[cols])
  })
}

# This function creates the store for one session.
# The version counter tells observers when the data changes.
new_data_store <- function(initial = data.frame(iris)) {
  store <- new.env(parent = emptyenv())
  
  store$df <- initial
  store$version <- reactiveVal(0)
  store$col_types <- reactiveVal(infer_col_types(initial))
  
  store$selected_cols <- reactiveVal(character(0))
  
  # This function adds 1 to the version counter.
  store$bump <- function() {
    store$version(isolate(store$version()) + 1)
  }
  
  # This function infers the column types again after new data arrives.
  store$reset_col_types <- function() {
    store$col_types(infer_col_types(store$df))
  }
  
  # This helper saves a new data frame.
  # It adds 1 to the version counter.
  # With reset_types = TRUE, it also infers the column types again.
  store$set_df <- function(new_df, reset_types = TRUE) {
    store$df <- new_df
    if (reset_types) {
      new_types <- infer_col_types(new_df)
      current_types <- store$col_types()
      # Write the types only when they change. Every write re-builds the
      # column selection panel, and the browser then loses the selection.
      if (!identical(new_types, current_types))
        store$col_types(new_types)
    }
    store$bump()
    invisible(NULL)
  }
  
  store
}
