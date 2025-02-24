
#' @export
flexCol <- function(cont, style = "") {
  shiny::tag("div", varArgs = list(cont, class = "flex-col-wrapper", style = style))
}

#' @export
flexRow <- function(cont, style = "") {
  shiny::tag("div", varArgs = list(cont, class = "flex-row-wrapper", style = style))
}
