# This file registers the two-way absolute frequency table with margins operation.
# It builds a cross table of counts, with row and column totals.
register_operation(
  "Absolute frequency table including marginal frequencies (margins)",
  # addmargins adds a "Sum" row and a "Sum" column to the table.
  fn = function(df, cols) addmargins(table(df[[cols[1]]], df[[cols[2]]])),
  types = c("nominal", "discrete"),
  min_cols = 2, max_cols = 2
)
