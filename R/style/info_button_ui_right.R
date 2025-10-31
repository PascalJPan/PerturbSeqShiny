info_button_right <- function(ns, id, label = "Info", icon = "info-circle", class = "info-btn") {
  column(
    width = 12,
    div(
      style = "display: flex; justify-content: flex-end;",
      actionButton(
        inputId = ns(id),
        label   = tagList(shiny::icon(icon), tags$span(label)),
        class   = class
      )
    )
  )
}