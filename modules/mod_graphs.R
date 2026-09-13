# This module shows the graph controls and draws the selected graph.
# It builds a modal window with graph options, colours, and resolution controls.
mod_graphs_ui <- function(id) {
  ns <- NS(id)
  accordion_panel(
    title = i18n$t("Graphs"),
    value = "graphs",
    uiOutput(ns("graph_choices")),
    actionButton(ns("draw"), i18n$t("Draw"), width = "100%")
  )
}

# CSS rules for the layout of the graph modal window.
GRAPH_MODAL_CSS <- r"(
.graph-modal-grid { display: grid; grid-template-columns: 300px 1fr; gap: 1em; width: 100%; height: 100%; }
.graph-modal-grid > .card { overflow-y: auto; min-height: 0; }
.graph-plot-wrap { overflow: auto; min-height: 0; }
.graph-plot-wrap img { max-width: 100%; height: auto; }
)"

# Return the correct button label for the regression line toggle.
regression_label <- function(showing) {
  i18n$t(if (showing)
    "Hide regression line"
    else
      "Show regression line")
}

# Build the output area of the modal window for the given render type.
graph_modal_body <- function(render, ns, cols_pair = NULL) {
  switch(
    render,
    table = tableOutput(ns("graph_table")),
    text  = verbatimTextOutput(ns("graph_text")),
    plot  = if (!is.null(cols_pair))
      tags$div(
        class = "graph-plot-wrap",
        plotOutput(ns("graph_plot"), width = "100%"),
        plotOutput(ns("graph_plot_rev"), width = "100%")
      )
    else
      tags$div(class = "graph-plot-wrap", plotOutput(ns("graph_plot"), width = "100%")),
    stop("Unknown render type: ", render)
  )
}

# Build the control panel of the modal window: parameters, colours,
# resolution field, and the redraw and download buttons.
build_modal_controls <- function(graph_def,
                                 values,
                                 ns,
                                 resolution,
                                 regression_shown) {
  controls <- tagList()
  if (length(graph_def$params)) {
    controls <- tagList(controls,
                        lapply(graph_def$params, function(param_spec)
                          render_param(param_spec, values[[param_spec$id]], ns)))
  }
  if (isTRUE(graph_def$colours)) {
    controls <- tagList(
      controls,
      tags$hr(),
      textInput(ns("colour_vec_text"), i18n$t("Colours"), placeholder = "#1f77b4, #d62728, #2ca02c"),
      actionButton(ns("set_colour_vec"), i18n$t("Set colour vector")),
      textOutput(ns("colour_vec_msg")),
      colourpicker::colourInput(ns("colour_pick"), i18n$t("Pick a colour"), "#1f77b4"),
      actionButton(ns("append_colour"), i18n$t("Add colour"))
    )
  }
  controls <- tagList(
    controls,
    tags$hr(),
    textInput(
      ns("resolution_text"),
      i18n$t("Resolution (width, height)"),
      value = paste(resolution, collapse = ", "),
      placeholder = "600, 400"
    ),
    textOutput(ns("resolution_msg"))
  )
  controls <- tagList(controls, actionButton(ns("redraw"), i18n$t("Redraw")))
  if (isTRUE(graph_def$regression_toggle)) {
    controls <- tagList(controls,
                        actionButton(
                          ns("toggle_regression"),
                          regression_label(regression_shown),
                          width = "100%"
                        ))
  }
  if (identical(graph_def$render, "plot")) {
    controls <- tagList(controls, actionButton(ns("download_plot"), i18n$t("Download"), width = "100%"))
  }
  controls
}

mod_graphs_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # spec holds the current graph name, columns, parameters, and render type.
    spec <- reactiveVal(NULL)
    
    colour_vec <- reactiveVal(character(0))
    colour_vec_msg <- reactiveVal(NULL)
    
    show_regression <- reactiveVal(FALSE)
    
    resolution <- reactiveVal(default_resolution)
    resolution_msg <- reactiveVal(NULL)
    
    # graph_data gives the current data frame and updates when the store changes.
    graph_data <- reactive({
      store$version()
      store$df
    })
    
    # Make a resource path for this session to serve downloaded graph files.
    download_prefix <- paste0("graphs-", session$token)
    download_dir <- file.path(tempdir(), session$token)
    dir.create(download_dir,
               showWarnings = FALSE,
               recursive = TRUE)
    addResourcePath(download_prefix, download_dir)
    session$onSessionEnded(function() {
      removeResourcePath(download_prefix)
      unlink(download_dir, recursive = TRUE)
    })
    
    selected_types <- store_selected_types(store)
    
    # valid_graphs lists the graphs that fit the types of the selected columns.
    valid_graphs <- reactive(applicable_graphs(selected_types()))
    
    # Show the radio buttons for the graphs that fit the current selection.
    output$graph_choices <- renderUI({
      session$userData$shiny.i18n$lang()
      translated_radio_input(
        ns,
        "graph",
        "Choose a graph",
        valid_graphs(),
        "Select column(s) in Column Selection to see available graphs.",
        current = isolate(input$graph)
      )
    })
    
    # Run the current graph. When rev_cols is TRUE, swap the two axis labels.
    run_current_graph <- function(rev_cols = FALSE) {
      current <- spec()
      params <- current$params
      if (rev_cols) {
        params$label1 <- current$params$label2
        params$label2 <- current$params$label1
      }
      tryCatch(
        run_graph(
          graph_data(),
          current$name,
          if (rev_cols)
            rev(current$cols)
          else
            current$cols,
          params
        ),
        error = function(e)
          stop(
            i18n$t("Error while drawing the graph"),
            ": ",
            conditionMessage(e),
            call. = FALSE
          )
      )
    }
    
    # preview_dims gives the plot size to show inside the modal window.
    preview_dims <- reactive(preview_resolution(resolution()))
    
    # Draw the main plot for the current graph spec.
    output$graph_plot <- renderPlot({
      current <- spec()
      req(current, current$render == "plot")
      run_current_graph()
    }, width = function()
      preview_dims()[1], height = function()
        preview_dims()[2])
    
    # Draw a second plot with the two selected columns in reverse order.
    output$graph_plot_rev <- renderPlot({
      current <- spec()
      req(current,
          current$render == "plot",
          length(current$cols) == 2)
      run_current_graph(rev_cols = TRUE)
    }, width = function()
      preview_dims()[1], height = function()
        preview_dims()[2])
    
    # Draw the graph result as a formatted table.
    output$graph_table <- renderTable({
      current <- spec()
      req(current, current$render == "table")
      run_current_graph() |>
        format_table_columns(function(col) {
          format(
            col,
            scientific = FALSE,
            drop0trailing = TRUE,
            trim = TRUE
          )
        })
    }, digits = NULL)
    
    # Draw the graph result as plain text.
    output$graph_text <- renderPrint({
      current <- spec()
      req(current, current$render == "text")
      cat(run_current_graph(), sep = "\n")
    })
    
    output$colour_vec_msg <- renderText(colour_vec_msg())
    output$resolution_msg <- renderText(resolution_msg())
    
    # Rebuild spec with fresh parameters, so the modal redraws with new values.
    refresh_spec <- function() {
      current <- spec()
      req(current)
      spec(
        list(
          name = current$name,
          cols = current$cols,
          params = current_params(),
          render = current$render
        )
      )
    }
    
    # Parse the resolution text field and store the size if it is valid.
    apply_resolution <- function() {
      parsed <- parse_resolution(input$resolution_text)
      if (parsed$ok) {
        resolution(parsed$size)
        resolution_msg(NULL)
      } else {
        resolution_msg(parsed$msg)
      }
      parsed$ok
    }
    
    # Open the modal window with the controls and output for one graph.
    show_graph_modal <- function(name, graph_def, values) {
      showModal(
        modalDialog(
          title = i18n$t(name),
          tags$style(paste(
            MODAL_SIZING_CSS, GRAPH_MODAL_CSS, sep = "\n"
          )),
          tags$div(
            class = "graph-modal-grid",
            card(card_body(
              build_modal_controls(graph_def, values, ns, resolution(), show_regression())
            )),
            graph_modal_body(graph_def$render, ns, if (identical(graph_def$render, "plot") &&
                                                       length(spec()$cols) == 2)
              spec()$cols
              else
                NULL)
          ),
          easyClose = TRUE,
          footer = modalButton(i18n$t("Close"))
        )
      )
    }
    
    # Collect the current input values and add colour, resolution, and
    # regression state to build the full parameter list for the graph.
    current_params <- function() {
      current <- spec()
      req(current)
      graph_def <- available_graphs[[current$name]]
      attach_graph_state(
        collect_params(graph_def$params, input),
        graph_def,
        resolution(),
        colour_vec(),
        show_regression()
      )
    }
    
    # Reset colours, resolution, and regression state before a new graph.
    reset_modal_state <- function() {
      colour_vec(character(0))
      colour_vec_msg(NULL)
      resolution(default_resolution)
      resolution_msg(NULL)
      show_regression(FALSE)
    }
    
    # When the user clicks Draw, check the selection and open the graph modal.
    observeEvent(input$draw, {
      cols <- store$selected_cols()
      graph <- input$graph
      
      if (is.null(graph) || !(graph %in% valid_graphs())) {
        info_modal("No graph available",
                   i18n$t("Select column(s) and a valid graph first."))
        return()
      }
      
      graph_def <- available_graphs[[graph]]
      reset_modal_state()
      params <- attach_graph_state(default_params(graph_def$params),
                                   graph_def,
                                   default_resolution)
      spec(list(
        name = graph,
        cols = cols,
        params = params,
        render = graph_def$render
      ))
      show_graph_modal(graph, graph_def, params)
    })
    
    # Parse the colour vector text field and save the colours if it is valid.
    observeEvent(input$set_colour_vec, {
      parsed <- parse_colour_vector(input$colour_vec_text)
      if (parsed$ok) {
        colour_vec(parsed$colours)
        colour_vec_msg(
          if (!length(parsed$colours))
            i18n$t("Colour vector is empty")
          else
            t_sprintf(
              "Colour vector set (%d colours)",
              length(parsed$colours)
            )
        )
      } else {
        colour_vec_msg(parsed$msg)
      }
      if (!is.null(spec()))
        refresh_spec()
    })
    
    # Add the picked colour to the colour vector text field.
    observeEvent(input$append_colour, {
      current <- trimws(rlang::`%||%`(input$colour_vec_text, ""))
      picked <- rlang::`%||%`(input$colour_pick, "")
      if (!nzchar(picked))
        return()
      value <- if (nzchar(current))
        paste(current, picked, sep = ", ")
      else
        picked
      updateTextInput(session, "colour_vec_text", value = value)
    })
    
    # Switch the regression line on or off and redraw the graph.
    observeEvent(input$toggle_regression, {
      req(spec())
      show_regression(!show_regression())
      updateActionButton(session, "toggle_regression", label = regression_label(show_regression()))
      refresh_spec()
    })
    
    # Apply the new resolution and redraw the graph with the current params.
    observeEvent(input$redraw, {
      apply_resolution()
      refresh_spec()
    })
    
    # Draw the graph to a PNG file. Return TRUE if it works, FALSE if not.
    render_graph_png <- function(file, current, params) {
      size <- resolution()
      grDevices::png(file, width = size[1], height = size[2])
      on.exit(grDevices::dev.off(), add = TRUE)
      tryCatch({
        result <- run_graph(graph_data(), current$name, current$cols, params)
        if (ggplot2::is_ggplot(result))
          print(result)
        TRUE
      }, error = function(e)
        FALSE)
    }
    
    # Save the plot as a PNG file and open it in a new browser tab.
    observeEvent(input$download_plot, {
      current <- spec()
      req(current, current$render == "plot")
      
      if (!apply_resolution())
        return()
      
      file <- tempfile(pattern = "plot-",
                       tmpdir = download_dir,
                       fileext = ".png")
      if (!render_graph_png(file, current, current_params())) {
        unlink(file)
        showNotification(i18n$t("Error while drawing the graph"), type = "error")
        return()
      }
      session$sendCustomMessage("openGraphTab", file.path(download_prefix, basename(file)))
    })
  })
}
