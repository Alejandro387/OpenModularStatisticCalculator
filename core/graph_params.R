# This file defines the parameter helpers for the graphs.
# It provides the parameter specs, the parsers, and the resolution helpers.

# This helper creates the spec of one numeric parameter.
p_num    <- function(id,
                     label,
                     default,
                     min_val = -Inf,
                     max_val = Inf,
                     step = NA) {
  list(
    id = id,
    type = "num",
    label = label,
    default = default,
    min = min_val,
    max = max_val,
    step = step
  )
}

# This helper creates the spec of one colour parameter.
p_colour <- function(id, label, default) {
  list(
    id = id,
    type = "colour",
    label = label,
    default = default
  )
}

# This helper creates the spec of one text parameter.
p_text   <- function(id, label, default = "") {
  list(
    id = id,
    type = "text",
    label = label,
    default = default
  )
}

# This helper builds the list of default values, named by parameter id.
default_params <- function(param_specs) {
  setNames(
    lapply(param_specs, function(spec)
      spec$default),
    vapply(param_specs, function(spec)
      spec$id, character(1))
  )
}

# This helper returns the value, or the fallback when the value is empty.
param_or <- function(value, fallback) {
  if (is.null(value) || length(value) != 1 ||
      !nzchar(trimws(as.character(value))))
    fallback
  else
    value
}

# This helper translates a format string and fills in the values.
# It returns the plain string when sprintf fails.
t_sprintf <- function(fmt, ...) {
  fmt <- i18n$t(fmt)
  tryCatch(
    sprintf(fmt, ...),
    error = function(e)
      fmt
  )
}

# This function parses the colour vector text.
# It returns the colours, or an error message.
parse_colour_vector <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(list(
      ok = TRUE,
      colours = character(0),
      msg = ""
    ))
  }
  tokens <- unlist(strsplit(trimws(text), "[,[:space:]]+"))
  is_colour <- vapply(tokens, function(token) {
    length(tryCatch(
      grDevices::col2rgb(token),
      error = function(e)
        NULL
    )) > 0
  }, logical(1))
  if (any(!is_colour)) {
    invalid_colours <- paste(tokens[!is_colour], collapse = ", ")
    return(list(
      ok = FALSE,
      colours = NULL,
      msg = t_sprintf("Not valid colours: %s", invalid_colours)
    ))
  }
  list(ok = TRUE,
       colours = tokens,
       msg = "")
}

# This function builds the input control for one parameter spec.
render_param <- function(spec, value, ns) {
  id <- ns(paste0("param_", spec$id))
  label <- i18n$t(spec$label)
  if (is.null(value))
    value <- spec$default
  
  switch(
    spec$type,
    num = {
      args <- list(inputId = id,
                   label = label,
                   value = value)
      if (is.finite(spec$min))
        args$min <- spec$min
      if (is.finite(spec$max))
        args$max <- spec$max
      if (!is.na(spec$step))
        args$step <- spec$step
      do.call(numericInput, args)
    },
    colour = colourpicker::colourInput(id, label, value = value),
    text = textInput(id, label, value = value)
  )
}

# This function reads the parameter inputs, and applies the defaults and the limits.
collect_params <- function(param_specs, input) {
  values <- lapply(param_specs, function(spec) {
    raw <- input[[paste0("param_", spec$id)]]
    switch(spec$type, num = {
      value <- suppressWarnings(as.numeric(raw))
      if (length(value) != 1 ||
          is.na(value))
        spec$default
      else
        max(spec$min, min(spec$max, value))
    }, if (is.null(raw) ||
           (is.character(raw) && !nzchar(raw)))
      spec$default
    else
      raw)
  })
  setNames(values, vapply(param_specs, function(spec)
    spec$id, character(1)))
}

# This helper adds the resolution, the colours, and the regression flag to the parameters.
attach_graph_state <- function(params,
                               graph_def,
                               resolution,
                               colour_vec = character(0),
                               show_regression = FALSE) {
  params$resolution <- resolution
  if (isTRUE(graph_def$colours))
    params$colours <- colour_vec
  if (isTRUE(graph_def$regression_toggle))
    params$regression_line <- show_regression
  params
}

# The default resolution of a new plot, in pixels.
default_resolution <- c(600, 400)

# The largest allowed resolution, in pixels.
max_resolution <- c(10000, 10000)

# The largest edge of the preview image, in pixels.
preview_max_edge <- 1000

# This function scales the resolution down so that the larger edge fits the limit.
# It never scales up.
preview_resolution <- function(size, max_edge = preview_max_edge) {
  size * min(1, max_edge / max(size[1], size[2]))
}

# This function parses the resolution text.
# Valid text holds two positive numbers, separated by a comma.
parse_resolution <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(list(ok = TRUE, size = default_resolution, msg = ""))
  }
  nums <- suppressWarnings(as.numeric(trimws(strsplit(text, ",")[[1]])))
  if (length(nums) == 2 && !anyNA(nums) &&
      all(nums == round(nums)) && all(nums > 0) &&
      all(nums <= max_resolution)) {
    list(ok = TRUE, size = nums, msg = "")
  } else {
    list(
      ok = FALSE,
      size = NULL,
      msg = t_sprintf(
        "Not a valid resolution: use two positive numbers separated by a comma, at most %d each",
        max_resolution[1]
      )
    )
  }
}

# This helper returns the user colours.
# When the colour vector is empty, it returns a default palette.
graph_colours <- function(colours, n_colours) {
  if (length(colours))
    colours
  else
    grDevices::hcl.colors(max(n_colours, 1))
}
