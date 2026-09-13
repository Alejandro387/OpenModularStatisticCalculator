# This file registers the interquartile range operation.
# It calculates the interquartile range of one column, with NA values removed.
register_operation(
  "Interquartile range",
  # type = 2 sets the quantile algorithm used by IQR.
  fn = function(df, cols) IQR(df[[cols[1]]],type=2, na.rm = TRUE),
  types = c("discrete", "continuous"),
  min_cols = 1, max_cols = 1
)
