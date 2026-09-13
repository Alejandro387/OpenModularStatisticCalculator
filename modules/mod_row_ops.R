# This module adds and deletes rows of the data table.

# The UI panel holds the add-row and delete-row controls.
mod_row_ops_ui <- function(id) {
  ns <- NS(id)
  accordion_panel(
    title = i18n$t("Row Operations"),
    value = "rows",
    actionButton(ns("add_row"), i18n$t("Add Row"), width = "100%"),
    hr(),
    numericInput(
      ns("delete_row_num"),
      i18n$t("Row number to delete"),
      value = 1,
      min = 1
    ),
    actionButton(ns("delete_row"), i18n$t("Delete Row"), width = "100%")
  )
}

mod_row_ops_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    # This handler adds one row of NA values to the data frame.
    observeEvent(input$add_row, {
      new_row <- setNames(as.data.frame(matrix(
        NA, nrow = 1, ncol = ncol(store$df)
      )), names(store$df))
      store$set_df(rbind(store$df, new_row))
    })
    
    # This handler deletes the row with the given number.
    # It keeps at least one row.
    observeEvent(input$delete_row, {
      row_num <- input$delete_row_num
      req(row_num >= 1, row_num <= nrow(store$df), nrow(store$df) > 1)
      store$set_df(store$df[-row_num, , drop = FALSE])
    })
    
    # This observer updates the maximum row number after each change.
    observe({
      store$version()
      updateNumericInput(session, "delete_row_num", max = max(nrow(store$df), 1))
    })
  })
}
