box::use(
  future[plan],
  shiny,
  shiny.router[route, router_server, router_ui],
  shinyFeedback[useShinyFeedback],
  shinyjs[useShinyjs],
  stringr[str_remove, str_detect],
)

box::use(
  app/view/index/academyIndex,
  app/view/index/careerRecords,
  app/view/index/leagueIndex,
  app/view/index/schedule,
  app/view/index/standings,
  app/view/navigationBar,
  app/view/tracker/draftclass,
  app/view/tracker/organization,
  app/view/tracker/player,
  app/view/welcome,
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
      route("index/schedule", schedule$ui(ns("schedule"))),
      route("index/academy", academyIndex$ui(ns("academy"))),
      route("tracker/organization", organization$ui(ns("organization"))),
      route("tracker/draftclass", draftclass$ui(ns("draftclass"))),
      route("tracker/player", player$ui(ns("player")))
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    
    plan("multisession")
    
    ## Reactives
    resAuth <- shiny$reactiveValues(
      uid = NULL,
      username = NULL,
      usergroup = NULL
    )

    # Adds all authentication list to a reactive object
    authOutput <- shiny$reactive({
      shiny$reactiveValuesToList(resAuth)
    })

    navigationBar$server("nav", auth = authOutput, resAuth = resAuth)

    router_server("/")

    ## Loads the different module servers
    welcome$server("message", usergroup = authOutput()$usergroup)


    ## In order to load pages as they are clicked ONCE this is needed
    loadedServer <-
      shiny$reactiveValues(
        create = FALSE, player = FALSE, index = FALSE,
        academy = FALSE, uploadGame = FALSE,
        bankOverview = FALSE, welcome = FALSE, records = FALSE,
        playerPages = FALSE, contractProcess = FALSE,
        tradeProcess = FALSE, playerEdit = FALSE, submitPT = FALSE,
        bankDeposit = FALSE, bankProcess = FALSE,
        standings = FALSE, schedule = FALSE, managerTeam = FALSE,
        assignManager = FALSE, bodoverview = FALSE, exportBuild = FALSE,
        organization = FALSE, draftclass = FALSE, nationTracker = FALSE,
        positionTracker = FALSE
      )

    ## Observer that checks the current page and loads the server for the page ONCE
    shiny$observe({
      current <- str_remove(session$clientData$url_hash,
                            pattern = "#!/")

      if (current == "index/records" & !loadedServer$records) {
        careerRecords$server("records")
        loadedServer$records <- TRUE
      } else if (current == "index/" & !loadedServer$index) {
        leagueIndex$server("league")
        loadedServer$index <- TRUE
      } else if (current == "index/standings" & !loadedServer$standings) {
        standings$server("standings")
        loadedServer$standings <- TRUE
      } else if (current == "index/schedule" & !loadedServer$schedule) {
        schedule$server("schedule")
        loadedServer$schedule <- TRUE
      } else if (current == "index/academy" & !loadedServer$academy) {
        academyIndex$server("academy")
        loadedServer$academy <- TRUE
      } else if (current == "tracker/organization" & !loadedServer$organization) {
        organization$server("organization")
        loadedServer$organization <- TRUE
      } else if (current == "tracker/draftclass" & !loadedServer$draftclass) {
        draftclass$server("draftclass")
        loadedServer$draftclass <- TRUE
      } else if (current |> str_detect("tracker/player") & !loadedServer$player) {
        player$server("player")
        loadedServer$player <- TRUE
      }
    }) |>
      shiny$bindEvent(session$clientData$url_hash)


  })
}
