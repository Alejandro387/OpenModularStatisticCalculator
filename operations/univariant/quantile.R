# This file registers the quantile operation.
# It calculates one or more quantiles of a column, at levels the user sets.
register_operation(
  "Quantile",
  fn = function(df, cols, params) {
    # params$probs holds the quantile levels the user sets, from parse_param.
    quantile(df[[cols[1]]], probs = params$probs, na.rm = TRUE)
  },
  types = c("discrete", "continuous"),
  min_cols = 1, max_cols = 1,
  takes_params = TRUE,
  # param_ui builds the text box where the user enters the percentile list.
  param_ui = function(ns) {
    textInput(
      ns("quantile_probs"),
      i18n$t("Percentiles (0-1, comma-separated)"),
      value = "0.25, 0.5, 0.75"
    )
  },
  # parse_param reads the user text and builds the list of valid probs.
  parse_param = function(input) {
    raw <- input$quantile_probs
    # Split the raw text on commas and change each part to a number.
    probs <- suppressWarnings(as.numeric(trimws(strsplit(raw, ",")[[1]])))
    # Keep only valid numbers between 0 and 1.
    probs <- probs[!is.na(probs) & probs >= 0 & probs <= 1]
    # If no valid value remains, use the median as the default.
    if (length(probs) == 0) probs <- 0.5
    list(probs = probs)
  }
)
