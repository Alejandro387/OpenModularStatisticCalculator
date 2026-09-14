# This module shows the editable data table.
# The user edits cells here, and the module writes the changes to the store.

# The registry places this module in the main area.
mod_table_where <- "main"
mod_table_order <- 0
mod_table_ui <- function(id) {
  ns <- NS(id)
  card(card_header(i18n$t("Data Table")), card_body(DT::DTOutput(ns("main_table"))))
}

mod_table_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    # This output shows the data frame as an editable DT table.
    output$main_table <- DT::renderDT({
      store$version()
      DT::datatable(
        store$df,
        selection = "none",
        editable = "cell",
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
    # The proxy applies the cell edits without a full table redraw.
    proxy <- DT::dataTableProxy("main_table")
    
    # This handler saves each cell edit, infers the column types again, and bumps the version.
    observeEvent(input$main_table_cell_edit, {
      cell_edit <- input$main_table_cell_edit
      store$set_df(DT::editData(
        store$df,
        cell_edit,
        proxy,
        resetPaging = FALSE,
        rownames = FALSE
      ))
    })
  })
}
