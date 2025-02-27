box::use(
  shiny,
  bslib,
  sass,
  shiny.router[router_ui, router_server, route, route_link],
  shinyFeedback[useShinyFeedback],
  shinyjs[useShinyjs],
)

box::use(
  app/view/welcome,
  app/view/index/careerRecords,
  app/view/navigationBar,
)


#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$bootstrapPage(
    useShinyFeedback(), # include shinyFeedback
    useShinyjs(), # include shinyjs
    title = "SSL Portal",
    
    navigationBar$ui(ns("nav")),
    # sidebar = "SIDEBAR",
    # theme = theme,
    router_ui(
      route("/", welcome$ui(ns("message"))),
      route("index/records", careerRecords$ui(ns("records")))
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ## Reactives
    resAuth <- shiny$reactiveValues(
      uid = NULL, 
      username = NULL, 
      usergroup = NULL
    )
    
    # Adds all authentication list to a reactive object
    authOutput <- shiny$reactive({shiny$reactiveValuesToList(resAuth)})
    
    
    navigationBar$server("nav", auth = authOutput, resAuth = resAuth)
    
    router_server("/")

    welcome$server("message", usergroup = 1)
    careerRecords$server("records")
  })
}
