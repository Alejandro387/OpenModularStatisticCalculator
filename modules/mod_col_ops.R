# This module lets the user add or delete columns in the data table.
# It shows a name field to add a new column and a select box to delete one.
mod_col_ops_ui <- function(id) {
  ns <- NS(id)
  accordion_panel(
    title = i18n$t("Column Operations"),
    value = "cols",
    textInput(ns("new_col_name"), i18n$t("New column name")),
    actionButton(ns("add_col"), i18n$t("Add Column"), width = "100%"),
    hr(),
    selectInput(ns("delete_col_name"), i18n$t("Column to delete"), choices = NULL),
    actionButton(ns("delete_col"), i18n$t("Delete Column"), width = "100%")
  )
}

mod_col_ops_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    # Add a new column to the store data frame with the type "nominal".
    # The module sets the type itself, so it skips the automatic type reset.
    observeEvent(input$add_col, {
      col_name <- trimws(input$new_col_name)
      req(nchar(col_name) > 0, !col_name %in% names(store$df))
      new_df <- store$df
      new_df[[col_name]] <- NA
      col_types <- store$col_types()
      col_types[col_name] <- "nominal"
      store$col_types(col_types)
      store$set_df(new_df, reset_types = FALSE)
      updateTextInput(session, "new_col_name", value = "")
    })
    
    # Delete the selected column, but keep at least one column in the table.
    # The module updates the types itself, so it skips the automatic type reset.
    observeEvent(input$delete_col, {
      col_name <- input$delete_col_name
      req(col_name, ncol(store$df) > 1)
      new_df <- store$df
      new_df[[col_name]] <- NULL
      col_types <- store$col_types()
      col_types <- col_types[names(col_types) != col_name]
      store$col_types(col_types)
      store$set_df(new_df, reset_types = FALSE)
    })
    
    # Refresh the drop-down list of columns for the delete action.
    observe({
      store$version()
      updateSelectInput(session, "delete_col_name", choices = names(store$df))
    })
  })
}
