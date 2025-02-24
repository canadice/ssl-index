box::use(
  shinycssloaders[withSpinner]
)

#' @export
withSpinnerCustom <- function(x, height) {
  withSpinner(ui_element = x, proxy.height = paste0(height, "px"), type = 8, size = 0.5)
}

