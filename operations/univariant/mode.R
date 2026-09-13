



# This file registers the mode operation.
# It calculates the mode of one column.
register_operation(

  "Mode",

  # stat_mode is a shared helper function, defined in another file.
  fn = function(df, cols) stat_mode(df[[cols[1]]]),

  types = c("discrete", "continuous", "nominal"),

  min_cols = 1, max_cols = 1

)

