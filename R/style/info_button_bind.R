info_button_bind <- function(session, input, btn_id, title, text = NULL, html = NULL) {
  # btn_id is the *un-namespaced* id you used inside ns(), e.g. "about_tf_btn"
  observeEvent(input[[btn_id]], {
    payload <- list(title = title)
    if (!is.null(html)) payload$html <- if (is.function(html)) html() else html
    if (!is.null(text)) payload$text <- if (is.function(text)) text() else text
    session$sendCustomMessage("show-info-sheet", payload)
  }, ignoreInit = TRUE)
}