flexCol <- function(cont, style = "") {
  shiny::tag("div", varArgs = list(cont, class = "flex-col-wrapper", style = style))
}

flexRow <- function(cont, style = "") {
  shiny::tag("div", varArgs = list(cont, class = "flex-row-wrapper", style = style))
}
