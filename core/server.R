# This file defines the server function of the app.
server <- function(input, output, session) {
  # Each session gets its own data store.
  store <- new_data_store()
  
  # This line sets the language at the start of the session.
  shiny.i18n::update_lang(i18n$get_translation_language(), session)
  
  # This observer applies the language that the user picks.
  observeEvent(input$selected_language, {
    shiny.i18n::update_lang(input$selected_language, session)
  })
  
  # This line starts the server of every module.
  start_module_servers(store)
}
