# This file registers the typical deviation operation.
# It calculates the population standard deviation of one column.
register_operation(
  "Typical deviation",
  fn = function(df, cols) {
    # Remove NA values from the column before the calculation.
    values <- df[[cols[1]]][!is.na(df[[cols[1]]])]
    n <- length(values)
    # Change the sample variance to the population variance, then take the square root.
    sqrt(var(values) * (n - 1) / n)
  },
  types = c("discrete", "continuous"),
  min_cols = 1, max_cols = 1
)
