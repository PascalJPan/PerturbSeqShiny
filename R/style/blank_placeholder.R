blank_placeholder <- function(height = "100%", text="Click the button above to render this UMAP") {
      div(style = paste0("height:", height, "; width:100%; ",
                     "display:flex; align-items:center; justify-content:center; ",
                     "border:1px dashed #ddd; color:#777;"),
      text)
}