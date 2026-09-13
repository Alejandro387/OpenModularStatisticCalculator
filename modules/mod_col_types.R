# This module lets the user set the measurement type of a column.
# The user picks a column and a type, and the module saves the choice to the store.
mod_col_types_ui <- function(id) {
  ns <- NS(id)
  accordion_panel(
    title = i18n$t("Column Types"),
    value = "types",
    selectInput(ns("type_col_name"), i18n$t("Column"), choices = NULL),
    selectInput(
      ns("type_value"),
      i18n$t("Type"),
      choices = c("nominal", "discrete", "continuous")
    ),
    actionButton(ns("set_type"), i18n$t("Set Type"), width = "100%")
  )
}

mod_col_types_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    # Save the new type for the selected column in the store.
    observeEvent(input$set_type, {
      column_name <- input$type_col_name
      new_type <- input$type_value
      req(column_name, new_type)
      col_types <- store$col_types()
      req(column_name %in% names(col_types))
      col_types[column_name] <- new_type
      store$col_types(col_types)
    })
    
    observe({
      # Update the list of columns shown in the type selector.
      updateSelectInput(session, "type_col_name", choices = names(store$col_types()))
    })
    
    # When the user picks a column, show its current type in the type field.
    observeEvent(input$type_col_name, {
      col_types <- store$col_types()
      selected_col <- input$type_col_name
      if (!is.null(selected_col) &&
          selected_col %in% names(col_types)) {
        updateSelectInput(session, "type_value", selected = col_types[selected_col])
      }
    })
    
    observe({
      # Refresh the type choices when the app language changes.
      session$userData$shiny.i18n$lang()
      updateSelectInput(
        session,
        "type_value",
        choices = setNames(
          c("nominal", "discrete", "continuous"),
          c(
            i18n$t("Nominal"),
            i18n$t("Discrete"),
            i18n$t("Continuous")
          )
        ),
        selected = isolate(input$type_value)
      )
    })
  })
}
