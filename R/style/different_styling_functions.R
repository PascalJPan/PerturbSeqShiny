section_header <- function(title) {
  shiny::tags$h2(title, class = "module-section-title", style = "margin-top:20px; ")
}

species_header <- function(title, color_preset = NULL) {
  if (is.null(color_preset) && title != "Human" && title != "Cynomolgus") {
    shiny::tags$h3(
      title,
      class = "module-section-title",
      style = "margin-top:20px; "
    )
    
  } else if (!is.null(color_preset) && color_preset == "human" || title == "Human") {
    shiny::tags$h3(
      title,
      class = "module-section-title",
      style = "margin-top:20px; color:var(--color-human); "
    )
    
  } else if (!is.null(color_preset) && color_preset == "cynomolgus" || title == "Cynomolgus") {
    shiny::tags$h3(
      title,
      class = "module-section-title",
      style = "margin-top:20px; color:var(--color-cynomolgus); "
    )
    
  } else if (!is.null(color_preset) && color_preset == "DR") {
    shiny::tags$h3(
      title,
      class = "module-section-title",
      style = "margin-top:20px; color:var(--color-DR); "
    )
    
  } else {
    stop(sprintf("This color preset '%s' is not available.", color_preset))
  }
}

styled_plot <- function(outputId, height = "100%", width="100%", spinner = TRUE) {
  if (spinner) {
    shinycssloaders::withSpinner(
      plotOutput(outputId, height = height, width = width),
      type = 6, color = "#444444", size = 1
    )
  } else {
    plotOutput(outputId, height = height, width = width)
  }
}

styled_ui <- function(outputId, height = "100%", width = "100%", spinner = TRUE) {
  container <- div(style = paste0("height:", height, "; width:", width, ";"),
                   uiOutput(outputId))
  if (spinner) {
    shinycssloaders::withSpinner(container, type = 6, color = "#444444", size = 1)
  } else {
    container
  }
}

styled_plotly_plot <- function(outputId, height = "100%", width="100%", spinner = TRUE) {
  if (spinner) {
    shinycssloaders::withSpinner(
      plotlyOutput(outputId, height = height, width = width),
      type = 6, color = "#444444", size = 1
    )
  } else {
    plotlyOutput(outputId, height = height, width = width)
  }
}

styled_table <- function(outputId, height = "auto", width = "100%") {
  shiny::tags$div(
    style = glue::glue("height: {height}; width: {width};"),
    DT::DTOutput(outputId)
  )
}
