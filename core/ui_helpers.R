# This file holds small helpers for the user interface.
# This helper translates the labels of a choices vector.
translated_choices <- function(choices) {
  setNames(choices, vapply(choices, i18n$t, character(1)))
}

# This helper builds a radio input with translated labels.
# It shows a message when there are no choices.
translated_radio_input <- function(ns,
                                   input_id,
                                   label_key,
                                   choices,
                                   empty_msg_key,
                                   current = NULL) {
  if (length(choices) == 0) {
    tags$em(i18n$t(empty_msg_key))
  } else {
    chosen <- intersect(choices, current)
    radioButtons(
      ns(input_id),
      i18n$t(label_key),
      choices = translated_choices(choices),
      selected = if (length(chosen))
        chosen
      else
        NULL
    )
  }
}

# CSS that makes the current modal fill most of the screen. All Shiny modals
# share one DOM element (#shiny-modal), and each modal's content is removed
# when it closes, so the rules must travel inside every modal that needs
# them: tags$style(MODAL_SIZING_CSS) within modalDialog().
MODAL_SIZING_CSS <- paste(
  "#shiny-modal .modal-dialog { --bs-modal-width: min(95vw, 1600px); height: 90vh; margin: 5vh auto; }",
  "#shiny-modal .modal-content { height: 100%; display: flex; flex-direction: column; }",
  "#shiny-modal .modal-body { flex: 1 1 0; overflow: hidden; }",
  sep = "\n"
)

# This helper shows a modal window with a message.
info_modal <- function(title_key, message) {
  showModal(modalDialog(
    title = i18n$t(title_key),
    message,
    easyClose = TRUE,
    footer = modalButton(i18n$t("Close"))
  ))
}

# This helper formats the numeric columns of a table.
format_table_columns <- function(df, digits_fn) {
  df[] <- lapply(df, function(col) {
    if (is.numeric(col))
      digits_fn(col)
    else
      col
  })
  df
}

# This helper converts an operation result to a data frame.
# It handles data frames, tables, arrays, and named vectors.
as_result_table <- function(value) {
  if (is.data.frame(value))
    return(value)
  if (is.table(value))
    value <- unclass(value)
  # A two-dimensional array becomes a table with the row names in the first column.
  if (is.array(value)) {
    if (length(dim(value)) == 2) {
      row_labels <- rlang::`%||%`(rownames(value), as.character(seq_len(nrow(value))))
      result_df <- cbind(" " = row_labels, as.data.frame(unclass(value)))
      names(result_df)[1] <- ""
      rownames(result_df) <- NULL
      return(result_df)
    }
    value <- setNames(as.vector(value), names(value))
  }
  named <- !is.null(names(value))
  result_df <- if (named)
    data.frame(label = names(value), value = unname(value))
  else
    data.frame(value = value)
  names(result_df) <- if (named)
    c("", i18n$t("Value"))
  else
    i18n$t("Value")
  result_df
}
