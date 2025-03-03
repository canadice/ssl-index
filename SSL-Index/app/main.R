box::use(
  shiny,
  shiny.router[router_ui, router_server, route, route_link, get_page, is_page],
  shinyFeedback[useShinyFeedback],
  shinyjs[useShinyjs],
  stringr[str_remove],
)

box::use(
  app/view/welcome,
  app/view/index/careerRecords,
  app/view/index/leagueIndex,
  app/view/navigationBar,
  app/view/index/standings,
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
      route("index/records", careerRecords$ui(ns("records"))),
      route("index/", leagueIndex$ui(ns("league"))),
      route("index/standings", standings$ui(ns("standings"))),
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
  
    ## Loads the different module servers
    welcome$server("message", usergroup = authOutput()$usergroup)
    
    
    ## In order to load pages as they are clicked ONCE this is needed
    loadedServer <- 
      shiny$reactiveValues(
        create = FALSE, player = FALSE, index = FALSE, academyIndex = FALSE, uploadGame = FALSE,
        bankOverview = FALSE, welcome = FALSE, records = FALSE, playerPages = FALSE, contractProcess = FALSE,
        tradeProcess = FALSE, playerEdit = FALSE, submitPT = FALSE, bankDeposit = FALSE, bankProcess = FALSE,
        standings = FALSE, leagueSchedule = FALSE, managerTeam = FALSE, assignManager = FALSE,
        bodoverview = FALSE, exportBuild = FALSE, organizationPages = FALSE, draftClass = FALSE,
        nationTracker = FALSE, positionTracker = FALSE
      )
    
    ## Observer that checks the current page and loads the server for the page ONCE
    shiny$observe({
      current <- str_remove(session$clientData$url_hash, 
                            pattern = "#!/")
      if(current == "index/records" & !loadedServer$records){
        careerRecords$server("records")
        loadedServer$records <- TRUE
      } else if(current == "index/" & !loadedServer$index){
        leagueIndex$server("league")
        loadedServer$index <- TRUE
      } else if(current == "index/standings" & !loadedServer$standings){
        standings$server("standings")
        loadedServer$standings <- TRUE
      }
    }) |> 
      shiny$bindEvent(session$clientData$url_hash)
    
    
  })
}
