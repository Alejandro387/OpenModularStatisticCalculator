

# This file registers the two-way relative frequency table with margins operation.
# It builds a cross table of proportions of the total, with row and column totals.
register_operation(

  "Relative frequency table including marginal frequencies",

  # prop.table with no margin uses the grand total for the proportions.
  fn = function(df, cols) addmargins(prop.table(table(df[[cols[1]]], df[[cols[2]]]))),

  types = c("nominal", "discrete"),

  min_cols = 2, max_cols = 2

)

