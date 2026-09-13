# This file registers the standard deviation operation.
# It calculates the standard deviation of one column, with NA values removed.
register_operation(
  "Standard deviation",
  fn = function(df, cols) sd(df[[cols[1]]], na.rm = TRUE),
  types = c("discrete", "continuous"),
  min_cols = 1, max_cols = 1
)
