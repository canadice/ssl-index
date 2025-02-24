box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput],
)

box::use(
  app/view/welcome,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    welcome$ui(ns("message"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    welcome$server("message", usergroup = 1)
  })
}
