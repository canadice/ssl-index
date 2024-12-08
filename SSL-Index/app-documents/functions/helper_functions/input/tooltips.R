## Functions for tooltips
withTooltip <- function (label, tooltip) 
{
  tags$span(
    class = "hovertext", 
    data_hover = tooltip, 
    label
  ) %>% 
    as.character() %>% 
    tooltipHTML()
}

tooltipHTML <- function (text, ...) 
{
  if (inherits(text, "shiny.tag.list")) {
    text <- as.character(text)
  }
  htmltools::HTML(text, ...)
}