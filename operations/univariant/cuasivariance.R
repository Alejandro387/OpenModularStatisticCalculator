# This file registers the cuasivariance operation.
# It calculates the sample variance of one column, with NA values removed.
register_operation(
  "Cuasivariance",
  fn = function(df, cols) var(df[[cols[1]]], na.rm = TRUE),
  types = c("discrete", "continuous"),
  min_cols = 1, max_cols = 1
)
