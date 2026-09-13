# This file registers the mean operation.
# It calculates the mean of one column, with NA values removed.
register_operation(
  "Mean",
  fn = function(df, cols) mean(df[[cols[1]]], na.rm = TRUE),
  types = c("discrete", "continuous"),
  min_cols = 1, max_cols = 1
)
