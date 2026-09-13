# This file registers the range operation.
# It returns the minimum and maximum value of one column.
register_operation(
  "Range",
  fn = function(df, cols) {
    values <- df[[cols[1]]]
    # range returns a vector with the minimum and the maximum, ignoring NAs.
    range(values, na.rm = TRUE)
  },
  types = c("discrete", "continuous"),
  min_cols = 1, max_cols = 1
)
