# This file registers the two-way conditional relative table with margins operation.
# It builds a cross table of proportions, with row and column totals added.
register_operation(
  "Conditional relative frequency table including marginal frequencies",
  # params$n sets the margin: 1 for row proportions, 2 for column proportions.
  fn = function(df, cols, params)
    addmargins(prop.table(table(df[[cols[1]]], df[[cols[2]]]), margin = params$n)),
  types = c("nominal", "discrete"),
  min_cols = 2, max_cols = 2,
  takes_params = TRUE,
  # margin_param_ui and parse_margin_param are shared helpers for the margin choice.
  # They are defined in another file.
  param_ui = margin_param_ui,
  parse_param = parse_margin_param
)
