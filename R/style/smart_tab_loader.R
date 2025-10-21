smart_tab_loader <- function(input, tab_id, module_id, server_fn, ...) {
  # Has this module been initialized yet?
  loaded <- shiny::reactiveVal(FALSE)
  
  # Reactive flag: is this tab currently active?
  is_active <- shiny::reactive({
    input$tabs == tab_id
  })
  
  # Initialize when the tab is (or becomes) active, but only once
  shiny::observe({
    if (is_active() && !loaded()) {
      server_fn(
        id = module_id,
        ...,
        is_active = is_active  # pass the same reactive to the module
      )
      loaded(TRUE)
    }
  })
}
