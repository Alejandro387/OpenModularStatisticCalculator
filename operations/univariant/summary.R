# This file registers the summary operation.
# It calculates the summary statistics of each selected column.
register_operation(
  "Summary",
  fn = function(df, cols) {
    # Remove NA values, run summary on each column, and round to 2 decimals.
    unlist(lapply(df[cols], function(column) round(summary(column[!is.na(column)]), 2)))
  },
  types = c("discrete", "continuous", "nominal"),
  min_cols = 1, max_cols = Inf
)
