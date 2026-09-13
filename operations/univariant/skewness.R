# This file registers the skewness operation.
# It calculates the skewness of one column.
register_operation(
  "Skewness",
  fn = function(df, cols) {
    # Remove NA values from the column before the calculation.
    values <- df[[cols[1]]][!is.na(df[[cols[1]]])]
    # skewness is a shared helper function, defined in another file.
    skewness(values)
  },
  types = c("discrete", "continuous"),
  min_cols = 1, max_cols = 1
)
