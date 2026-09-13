# This file registers the sum operation.
# It calculates the sum of one column, with NA values removed.
register_operation(
  "Sum",
  fn = function(df, cols) sum(df[[cols[1]]], na.rm = TRUE),
  types = c("discrete", "continuous"),
  min_cols = 1, max_cols = 1
)
