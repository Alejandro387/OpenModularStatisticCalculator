# This module shows the operation controls and runs the chosen operation.
# It shows the result as a single value or as a table in a modal window.
mod_operations_ui <- function(id) {
  ns <- NS(id)
  accordion_panel(
    title = i18n$t("Operations"),
    value = "operations",
    uiOutput(ns("operation_choices")),
    uiOutput(ns("operation_params")),
    actionButton(ns("proceed"), i18n$t("Proceed"), width = "100%")
  )
}

mod_operations_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # result holds the value that the last operation returns.
    result <- reactiveVal(NULL)
    last_decimals <- reactiveVal(4)  # last decimal-places choice, kept between runs
    
    # The types of the selected columns control which operations appear.
    selected_types <- store_selected_types(store)
    
    # available_ops lists the operations that fit the selected column types.
    available_ops <- reactive(applicable_operations(selected_types()))
    
    # The radio buttons list the available operations.
    # The output draws again after a language change.
    output$operation_choices <- renderUI({
      session$userData$shiny.i18n$lang()
      translated_radio_input(
        ns,
        "operation",
        "Choose an operation",
        available_ops(),
        "Select column(s) in Column Selection to see available operations."
      )
    })
    
    # This output shows the parameter inputs of the chosen operation.
    # It shows nothing when the operation takes no parameters.
    output$operation_params <- renderUI({
      session$userData$shiny.i18n$lang()
      operation_name <- input$operation
      req(operation_name, operation_name %in% available_ops())
      param_ui <- stat_operations[[operation_name]]$param_ui
      if (is.null(param_ui))
        return(NULL)
      param_ui(ns)
    })
    
    # This reactive holds the decimal places for the result.
    # It uses the last valid value when the input is not valid.
    decimals <- reactive({
      n_decimals <- suppressWarnings(as.integer(input$decimals))
      if (length(n_decimals) != 1 ||
          is.na(n_decimals) ||
          n_decimals < 0)
        last_decimals()
      else
        n_decimals
    })
    
    # This observer saves the last valid decimal-places value.
    observeEvent(input$decimals, {
      req(!is.null(input$decimals), input$decimals >= 0)
      last_decimals(input$decimals)
    })
    
    # This reactive builds the result table.
    # It rounds the numeric columns to the chosen decimal places.
    result_table <- reactive({
      session$userData$shiny.i18n$lang()
      n_decimals <- decimals()
      value <- result()
      if (is.null(value))
        return(NULL)
      format_table_columns(as_result_table(value), function(col)
        format(
          round(col, n_decimals),
          trim = TRUE,
          scientific = FALSE
        ))
    })
    
    # This reactive is TRUE when the result is one unnamed value.
    is_scalar_result <- reactive({
      value <- result()
      ! is.null(value) &&
        is.null(dim(value)) &&
        length(value) == 1 && is.null(names(value))
    })
    
    # The modal shows a single value, or a table for a larger result.
    output$op_result <- renderUI({
      session$userData$shiny.i18n$lang()
      value <- result()
      n_decimals <- decimals()
      if (is.null(value))
        return(NULL)
      if (is_scalar_result()) {
        if (is.numeric(value))
          value <- round(value, n_decimals)
        tags$strong(format(value, digits = 10, trim = TRUE))
      } else {
        tableOutput(ns("op_table"))
      }
    })
    
    # This output renders the result table.
    output$op_table <- renderTable({
      result_table()
    })
    
    # The Proceed button runs the chosen operation and opens the result modal.
    observeEvent(input$proceed, {
      cols <- store$selected_cols()
      operation_name <- input$operation
      
      if (is.null(operation_name) ||
          !(operation_name %in% available_ops())) {
        info_modal(
          "No operation available",
          i18n$t("Select column(s) and a valid operation first.")
        )
        return()
      }
      
      parse_param <- stat_operations[[operation_name]]$parse_param
      params <- if (is.null(parse_param))
        list()
      else
        parse_param(input)
      
      result(run_operation(store$df, operation_name, cols, params))
      showModal(
        modalDialog(
          title = i18n$t(operation_name),
          tags$style(MODAL_SIZING_CSS),
          tags$p(style = "color: #777; margin-bottom: 1em;", paste(cols, collapse = ", ")),
          fluidRow(column(
            width = 4,
            numericInput(
              ns("decimals"),
              i18n$t("Decimal places"),
              value = last_decimals(),
              min = 0,
              max = 15,
              step = 1
            )
          )),
          uiOutput(ns("op_result")),
          easyClose = TRUE,
          footer = modalButton(i18n$t("Close"))
        )
      )
    })
  })
}
