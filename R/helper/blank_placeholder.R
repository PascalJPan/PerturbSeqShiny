blank_placeholder <- function(height = "70vh") {
  div(style = paste0("height:", height, "; width:100%; ",
                     "display:flex; align-items:center; justify-content:center; ",
                     "border:1px dashed #ddd; color:#777;"),
      "Click the button above to render this UMAP")
}