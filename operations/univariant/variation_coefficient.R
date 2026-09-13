# This file registers the variation coefficient operation.
# It calculates the ratio of the standard deviation to the mean of one column.
register_operation(
  "Variation coefficient",
  fn = function(df, cols) {
    values <- df[[cols[1]]]
    # Divide the standard deviation by the mean.
    sd(values, na.rm = TRUE) / mean(values, na.rm = TRUE)
  },
  types = c("discrete", "continuous"),
  min_cols = 1, max_cols = 1
)
