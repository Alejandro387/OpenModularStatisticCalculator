# This file registers the median operation.
# It calculates the median of one column, with NA values removed.
register_operation(
  "Median",
  fn = function(df, cols) median(df[[cols[1]]], na.rm = TRUE),
  types = c("discrete", "continuous"),
  min_cols = 1, max_cols = 1
)
