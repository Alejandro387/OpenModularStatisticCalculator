# This file creates the translator for the app.
# The key language is English.
library(shiny.i18n)

# This cache holds the discovered translation tables, keyed by folder.
.translation_cache <- new.env(parent = emptyenv())

# This helper appends the rows of one table to another.
# Columns that only one of the tables has are filled with NA.
.append_rows <- function(table, rows) {
  for (col in setdiff(names(rows), names(table)))
    table[[col]] <- NA
  for (col in setdiff(names(table), names(rows)))
    rows[[col]] <- NA
  rbind(table, rows[, names(table), drop = FALSE])
}

# This function finds and loads the translation tables.
# Each first-level folder of translations/ holds one language. The
# standard translation_<code>.csv file of the folder is loaded first;
# the discovery then starts, finds any other .csv in the folder, and
# appends it to the standard table. A key defined more than once keeps
# its last definition. The function returns a list of tables named by
# the language code.
discover_translations <- function(dir = NULL, refresh = FALSE) {
  if (is.null(dir))
    dir <- app_file("translations")
  if (!dir.exists(dir))
    stop("Translations folder not found; no translations loaded.")
  if (!refresh &&
      !is.null(.translation_cache[[dir]]))
    return(.translation_cache[[dir]])
  
  # This first pass loads the standard table of every language folder.
  folders <- list.dirs(dir, full.names = TRUE, recursive = FALSE)
  languages <- list()
  for (folder in folders) {
    name <- basename(folder)
    # The standard file is the translation_*.csv of the folder, and its
    # name carries the language code, e.g. translation_es.csv -> es.
    base <- list.files(folder, pattern = "^translation_.*[.]csv$", full.names = TRUE)
    if (!length(base)) {
      warning("Skipping language folder '",
              name,
              "': no translation_*.csv base file.")
      next
    }
    code <- sub("^translation_(.*)[.]csv$", "\\1", basename(base[1]))
    if (length(base) > 1) {
      warning(
        "Language folder '",
        name,
        "' has several translation_*.csv files; using '",
        basename(base[1]),
        "'."
      )
    }
    if (!nzchar(code)) {
      warning("Skipping language folder '",
              name,
              "': the base file name has no language code.")
      next
    }
    table <- read.csv(base[1], header = TRUE, encoding = "UTF-8")
    if (!(code %in% names(table))) {
      warning("Skipping language folder '",
              name,
              "': the base file has no '",
              code,
              "' column.")
      next
    }
    languages[[code]] <- list(folder = folder,
                              base = basename(base[1]),
                              table = table)
  }
  
  # This second pass starts the discovery: it finds the other .csv files
  # of each folder and appends them to the standard table of the language.
  for (code in names(languages)) {
    language <- languages[[code]]
    key <- names(language$table)[1]
    extras <- setdiff(
      list.files(
        language$folder,
        pattern = "[.]csv$",
        full.names = TRUE
      ),
      file.path(language$folder, language$base)
    )
    for (extra in extras) {
      rows <- read.csv(extra, header = TRUE, encoding = "UTF-8")
      if (!(key %in% names(rows))) {
        warning("Skipping '",
                basename(extra),
                "': it has no '",
                key,
                "' key column.")
        next
      }
      language$table <- .append_rows(language$table, rows)
    }
    # A key defined several times keeps its last translation.
    if (anyDuplicated(language$table[[key]])) {
      warning(
        "In language folder '",
        basename(language$folder),
        "', some keys are defined more than once; the last definition wins."
      )
    }
    languages[[code]] <- language
  }
  
  # This block drops the repeated keys, keeping the last definition.
  tables <- lapply(languages, function(language) {
    key <- names(language$table)[1]
    language$table[!duplicated(language$table[[key]], fromLast = TRUE), , drop = FALSE]
  })
  
  .translation_cache[[dir]] <- tables
  tables
}

# This function builds the app Translator from the discovered tables.
# Each language table is written to a temporary folder, which the
# Translator merges into one translation set.
build_translator <- function(dir = NULL, refresh = FALSE) {
  tables <- discover_translations(dir = dir, refresh = refresh)
  if (!length(tables))
    stop("No language folders found in '", dir, "'.")
  
  # The Translator merges the tables by their key column, so all of
  # them must share it, e.g. "en".
  keys <- vapply(tables, function(table)
    names(table)[1], character(1))
  if (length(unique(keys)) > 1) {
    stop(
      "All translation tables must share the same key column: ",
      paste(unique(keys), collapse = ", "),
      "."
    )
  }
  
  tmp <- tempfile("i18n_")
  dir.create(tmp)
  for (code in names(tables)) {
    write.csv(
      tables[[code]],
      file.path(tmp, paste0("translation_", code, ".csv")),
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
  }
  Translator$new(translation_csvs_path = tmp)
}

# This line builds the shared translator from the translations folder.
i18n <- build_translator()
# The key column of the translation files holds the English strings.
i18n$set_translation_language("en")

# This line turns on the translation in the browser.
i18n$use_js()
