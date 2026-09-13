# This file registers the boxplot graph.
# The function draws one boxplot for the selected column.
register_graph(
  "Boxplot",
  fn = function(df, cols, params) {
    ggplot(df, aes(y = .data[[cols[1]]])) +
      geom_boxplot(fill = graph_colours(params$colours, 1)[1], na.rm = TRUE) +
      labs(title = param_or(params$title, NULL),
           y = param_or(params[["label1"]], cols[1])) +
      # Hide the x axis text and ticks, because the plot has one box only.
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  },
  types = c("discrete", "continuous"),
  min_cols = 1,
  max_cols = 1,
  params = list(p_text("title", "Title", ""), p_text("label1", "Label 1", "")),
  colours = TRUE
)
