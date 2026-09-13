# This module shows the column selection panel.
# The user selects columns by type, and the module saves the selection in the store.

# The registry places this module in the main area.
mod_type_summary_where <- "main"
mod_type_summary_ui <- function(id) {
  ns <- NS(id)
  card(card_header(i18n$t("Column Selection")), card_body(uiOutput(ns(
    "col_type_checkboxes"
  ))))
}

mod_type_summary_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # This output groups the columns by type, with one checkbox group per type.
    output$col_type_checkboxes <- renderUI({
      session$userData$shiny.i18n$lang()
      # Keep the user selection across rebuilds of the checkbox panel.
      previous_selection <- isolate(store$selected_cols())
      col_types <- store$col_types()
      type_labels <- c(
        discrete   = i18n$t("Discrete"),
        continuous = i18n$t("Continuous"),
        nominal    = i18n$t("Nominal")
      )
      fluidRow(lapply(names(type_labels), function(type) {
        cols <- names(col_types[col_types == type])
        column(width = 4, card(card_header(type_labels[type]), card_body(if (length(cols) == 0) {
          tags$em(i18n$t("No columns"))
        } else {
          checkboxGroupInput(
            inputId = ns(paste0("check_", type)),
            label = NULL,
            choices = cols,
            selected = intersect(cols, previous_selection)
          )
        })))
      }))
    })
    
    # This observer saves the selected columns in the store.
    observe({
      selected_columns <- c(input$check_discrete,
                            input$check_continuous,
                            input$check_nominal)
      store$selected_cols(if (is.null(selected_columns))
        character(0)
        else
          selected_columns)
    })
  })
}
