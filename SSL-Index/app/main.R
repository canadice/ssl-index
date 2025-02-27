box::use(
  shiny[NS, tags, bootstrapPage, moduleServer, a, icon, div, tagList, uiOutput, reactive, reactiveValues, reactiveValuesToList],
  bslib,
  sass,
  shiny.router[router_ui, router_server, route, route_link],
  shinyFeedback[useShinyFeedback],
  shinyjs[useShinyjs],
)

box::use(
  app/view/welcome,
  app/view/navigationBar,
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    useShinyFeedback(), # include shinyFeedback
    useShinyjs(), # include shinyjs
    title = "SSL Portal",
    
    navigationBar$ui(ns("nav")),
    # sidebar = "SIDEBAR",
    # theme = theme,
    router_ui(
      route("/", welcome$ui(ns("message")))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ## Reactives
    resAuth <- reactiveValues(
      uid = NULL, 
      username = NULL, 
      usergroup = NULL
    )
    
    # Adds all authentication list to a reactive object
    authOutput <- reactive({reactiveValuesToList(resAuth)})
    
    
    navigationBar$server("nav", auth = authOutput, resAuth = resAuth)
    
    router_server("/")

    welcome$server("message", usergroup = 1)
  })
}
