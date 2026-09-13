# This file registers the correlation operation.
# It calculates the Pearson correlation between two columns.
register_operation(
  "Correlation",
  # use = "complete.obs" drops rows where either value is NA.
  fn = function(df, cols) cor(df[[cols[1]]], df[[cols[2]]], use = "complete.obs"),
  types = c("discrete", "continuous"),
  min_cols = 2, max_cols = 2
)
