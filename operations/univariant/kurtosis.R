# This file registers the kurtosis operation.
# It calculates the kurtosis of one column.
register_operation(
  "Kurtosis",
  fn = function(df, cols) {
    # Remove NA values from the column before the calculation.
    values <- df[[cols[1]]][!is.na(df[[cols[1]]])]
    # kurtosis is a shared helper function, defined in another file.
    kurtosis(values)
  },
  types = c("discrete", "continuous"),
  min_cols = 1, max_cols = 1
)
