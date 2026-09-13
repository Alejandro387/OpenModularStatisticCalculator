# This module uploads a CSV file and loads it into the store.
# It can parse the file automatically, or the user can set the parsing options.

# The registry sorts this module first.
mod_upload_order <- 1


# This function detects the encoding of the file.
# It uses UTF-8 when the file is valid UTF-8, and windows-1252 otherwise.
detect_encoding <- function(path) {
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  txt <- rawToChar(bytes)
  Encoding(txt) <- "UTF-8"
  if (anyNA(iconv(txt, from = "UTF-8", to = "UTF-8")))
    "windows-1252"
  else
    "UTF-8"
}

# This function finds the column separator.
# It tests candidates and picks the separator that splits all lines into the same number of fields.
detect_sep <- function(lines) {
  candidates <- c(";", "\t", ",", "|", " ")
  score <- function(sep) {
    counts <- vapply(lines, function(line)
      length(strsplit(line, sep, fixed = TRUE)[[1]]), integer(1))
    if (counts[1] > 1 && all(counts == counts[1]))
      counts[1]
    else
      0L
  }
  scores <- vapply(candidates, score, integer(1))
  if (max(scores) == 0)
    ","
  else
    candidates[which.max(scores)]
}

# This function finds the decimal mark.
# When the separator is a comma, the decimal mark is a period.
detect_dec <- function(lines, sep) {
  if (identical(sep, ","))
    return(".")
  cells <- trimws(unlist(strsplit(lines, sep, fixed = TRUE)))
  comma_count  <- sum(grepl("^-?[0-9]+,[0-9]+$", cells))
  period_count <- sum(grepl("^-?[0-9]+\\.[0-9]+$", cells))
  if (comma_count > period_count)
    ","
  else
    "."
}

# This function reads a CSV file with automatic settings.
# It detects the encoding, the separator, and the decimal mark.
auto_read_csv <- function(path, header) {
  encoding <- detect_encoding(path)
  con <- file(path, encoding = encoding)
  on.exit(close(con))
  lines <- readLines(con, n = 50L, warn = FALSE)
  lines <- lines[nzchar(lines)]
  sep <- detect_sep(lines)
  dec <- detect_dec(lines, sep)
  read.csv(
    path,
    header = header,
    sep = sep,
    dec = dec,
    row.names = NULL,
    fileEncoding = encoding,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}
# The UI panel holds the file input and the parsing options.
mod_upload_ui <- function(id) {
  ns <- NS(id)
  accordion_panel(
    title = i18n$t("Upload File"),
    value = "upload",
    fileInput(
      ns("file_upload"),
      i18n$t("Choose CSV File"),
      accept = c(".csv", ".txt")
    ),
    checkboxInput(ns("header"), i18n$t("File has header"), TRUE),
    checkboxInput(ns("auto"), i18n$t("Automatic parsing"), TRUE),
    conditionalPanel(
      condition = sprintf("!input['%s']", ns("auto")),
      selectInput(
        ns("encoding"),
        i18n$t("File encoding"),
        choices = c(
          "UTF-8" = "UTF-8",
          "Latin-1 (ISO-8859-1)" = "latin1",
          "Windows-1252" = "windows-1252"
        ),
        selected = "UTF-8"
      ),
      selectInput(
        ns("sep"),
        i18n$t("Column separator"),
        choices = c(
          "Comma (,)" = ",",
          "Semicolon (;)" = ";",
          "Tab" = "\t",
          "Space" = " ",
          "Other" = "other"
        ),
        selected = ","
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'other'", ns("sep")),
        textInput(ns("sep_other"), i18n$t("Custom separator"), value = "|")
      ),
      selectInput(
        ns("dec"),
        i18n$t("Decimal mark"),
        choices = c("Period (.)" = ".", "Comma (,)" = ","),
        selected = "."
      )
    )
  )
}

mod_upload_server <- function(id, store) {
  moduleServer(id, function(input, output, session) {
    # This handler reads the uploaded file and saves the data in the store.
    observeEvent(input$file_upload, {
      file <- input$file_upload
      ext <- tolower(tools::file_ext(file$datapath))
      validate(need(
        ext %in% c("csv", "txt"),
        i18n$t("Please upload a CSV/TXT file")
      ))
      
      # Automatic mode detects the format.
      # Manual mode uses the chosen options.
      if (isTRUE(input$auto)) {
        store$set_df(auto_read_csv(file$datapath, input$header))
      } else {
        sep <- if (input$sep == "other")
          input$sep_other
        else
          input$sep
        validate(need(
          nzchar(sep),
          i18n$t("Please provide a custom separator")
        ))
        
        store$set_df(
          read.csv(
            file$datapath,
            header = input$header,
            sep = sep,
            dec = input$dec,
            row.names = NULL,
            fileEncoding = input$encoding,
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
        )
      }
    })
    
    # This observer updates the option labels after a language change.
    observe({
      session$userData$shiny.i18n$lang()
      updateSelectInput(session,
                        "sep",
                        choices = setNames(
                          c(",", ";", "\t", " ", "other"),
                          c(
                            i18n$t("Comma (,)"),
                            i18n$t("Semicolon (;)"),
                            i18n$t("Tab"),
                            i18n$t("Space"),
                            i18n$t("Other")
                          )
                        ),
                        selected = isolate(input$sep))
      updateSelectInput(session,
                        "dec",
                        choices = setNames(c(".", ","), c(
                          i18n$t("Period (.)"), i18n$t("Comma (,)")
                        )),
                        selected = isolate(input$dec))
    })
  })
}
