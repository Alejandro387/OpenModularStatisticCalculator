# This file registers the population variance operation.
# It calculates the population variance of one column.
register_operation(
  "Population variance",
  fn = function(df, cols) {
    # Remove NA values from the column before the calculation.
    values <- df[[cols[1]]][!is.na(df[[cols[1]]])]
    n <- length(values)
    # The population variance divides the squared deviations by n.
    sum((values - mean(values))^2) / n
  },
  types = c("discrete", "continuous"),
  min_cols = 1, max_cols = 1
)
