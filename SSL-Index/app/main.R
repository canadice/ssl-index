box::use(
  shiny,
  bslib,
  sass,
  shiny.router[router_ui, router_server, route, route_link],
)

box::use(
  app/view/welcome,
)

theme <- bslib$bs_theme(primary = "purple") |>
  bslib$bs_add_rules(sass$sass_file("app/styles/main.scss"))

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  bslib$page_fillable(
    title = "SSL Portal",
    # sidebar = "SIDEBAR",
    theme = theme,
    router_ui(
      route("welcome", welcome$ui(ns("message")))
    )
    
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    router_server("welcome")
    
    welcome$server("message", usergroup = 1)
  })
}
