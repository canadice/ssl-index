
###########################################################################
###########################################################################
###                                                                     ###
###                              SSL INDEX                              ###
###                                                                     ###
###########################################################################
###########################################################################

suppressMessages({
  ## Data handling
  require(dplyr, quietly = FALSE)
  require(tidyr, quietly = FALSE)
  require(purrr, quietly = FALSE)
  require(arsenal, quietly = FALSE)
  
  ## Visualizations
  require(ggplot2, quietly = FALSE)
  require(ggnewscale, quietly = FALSE)
  require(RColorBrewer, quietly = FALSE)
  require(cowplot, quietly = FALSE)
  require(plotly, quietly = FALSE)
  require(ggimage, quietly = FALSE)
  require(magick, quietly = FALSE)
  require(rsvg, quietly = FALSE)
  require(grid, quietly = FALSE)
  require(ggpubr, quietly = FALSE)
  require(ggforce, quietly = FALSE)
  
  ## Tables
  require(formattable, quietly = FALSE)
  # require(gt, quietly = FALSE)
  require(gtExtras, quietly = FALSE) #Github package
  require(reactable, quietly = FALSE)
  require(reactable.extras, quietly = FALSE)
  require(reactablefmtr, quietly = FALSE)
  require(tippy, quietly = FALSE)
  
  ## Package for handling date and time
  require(lubridate, quietly = FALSE)
  
  ## Packages for handling strings
  require(stringr, quietly = FALSE)
  require(stringi, quietly = FALSE)
  
  ## Loading packages for handling RMarkdown files
  require(rmarkdown, quietly = FALSE)
  require(markdown, quietly = FALSE)
  
  ##Loading Database packages for MySQL database
  require(DBI, quietly = FALSE)
  require(dbplyr, quietly = FALSE)
  require(RMySQL, quietly = FALSE)
  
  ## Loading jsonlite and httr for API calls
  require(jsonlite, quietly = FALSE)
  require(httr, quietly = FALSE)
  
  ## Loading Shiny packages
  require(shiny, quietly = FALSE)
  require(knitr, quietly = FALSE)
  require(kableExtra, quietly = FALSE)
  # require(shinythemes, quietly = FALSE)
  require(shinycssloaders, quietly = FALSE)
  require(shinyjs, quietly = FALSE)
  require(shinydashboard, quietly = FALSE)
  require(shinyBS, quietly = FALSE)
  
  require(fresh, quietly = FALSE)
  require(shiny.router, quietly = FALSE)
  
  require(vembedr, quietly = FALSE)
  
  ## Package for login
  require(sodium, quietly = FALSE)
  require(shinymanager, quietly = FALSE)
})


##################################################################
##                      SSL Logo and Theme                      ##
##################################################################

sslBlueD <- "#070B51"
sslBlueL <- "#141204"
sslGold <- "#BD9523"

customTheme <- 
  create_theme(
    adminlte_color(
      light_blue = sslBlueD,
      navy = sslBlueD,
      blue = sslBlueD,
      # Danger
      red = "#0F0F0F",
      # Success
      green = "#FFFFFF",
      # Warning
      yellow = "#0F0F0F",
      # Aqua
      aqua = "#0F0F0F"
    ),
    adminlte_sidebar(
      # width = "400px",
      dark_bg = sslBlueD,
      dark_hover_bg = sslGold,
      dark_color = "#fff",
      dark_submenu_bg = sslBlueD,
      dark_submenu_hover_color = sslGold,
      dark_submenu_color = "#fff"
    ),
    adminlte_global(
      content_bg = "#FFF",
      box_bg = "#D8DEE9", 
      info_box_bg = "#D8DEE9"
    )
  )
  


#################################################################
##               Running all modules for the app               ##
#################################################################

fileSources <- c("app-documents")

## Loads files
sapply(
  X = fileSources,
  FUN = function(x){
    files <- list.files(path = x, pattern = ".R$", recursive = TRUE)
    
    sapply(
      X = paste(x, files, sep = "/"),
      FUN = source
    )
  }
)

##################################################################
##                  The UI and Server function                  ##
##################################################################


ui <- function(request){
  dashboardPage(
    ##----------------------------------------------------------------
    ##                            Header                             -
    ##----------------------------------------------------------------
    
    dashboardHeader(
      title = "SSL Index",
      tags$li(
        tags$head(
          tags$link(
            rel = "icon", 
            type = "image/png", 
            href = "favicon.png"),
          tags$title("SSL Index")
        ),
        class = "dropdown",
        tags$head(
          ## HTML code so that a href link inherits the text color, not the link color
          tags$style(HTML("a, a:hover, a:visited, a:active {color: inherit}")),
          tags$style(HTML('
            thead th {
              background-color:#00044d !important;
              color:#ffffff !important;
            }')),
          tags$style(
            type="text/css",
            "#playerComparison-fieldImage img {max-width: 480px; width: inherit; max-height: 600px;}"
          )
          # ## Increases the size of the logo box at the top left
          # tags$style(".main-header {max-height: 80px}"),
          # tags$style(".main-header .logo {height: 80px}"),
          # tags$style(".main-header .logo {width: 300px}"),
          # 
          # ## Changes the margin of the sidebar
          # tags$style(".main-header .navbar {margin-left: 300px}"),
          # tags$style(type="text/css", "text {font-family: sans-serif, courier}"),
          # 
        )
      )
    ),
    dashboardSidebar(
      uiOutput("sidebarpanel")
    ),
    dashboardBody(
      customTheme %>% use_theme(),
      includeCSS('style.css'),
      useShinyjs(),
      uiOutput("body")
    )
  )
}

## Adds shinymanager authentication to the app
ui <- secure_app(ui)

server <- function(input, output, session) {
  
  resAuth <- secure_server(
    check_credentials = customCheckCredentials(),
    session = session
  )
  
  authOutput <- reactive({
    reactiveValuesToList(resAuth)
  })
  
  ##----------------------------------------------------------------
  ##                              Body                             -
  ##----------------------------------------------------------------
  output$body <- renderUI({
    tabItems(
      tabItem(
        "playerpage",
        playerPageUI(id = "playerpage"),
        class = "active"
      ),
      tabItem(
        "leagueindex",
        leagueIndexUI(id = "leagueindex")
      # ),
      # tabItem(
      #   "teamindex",
      #   teamIndexUI(id = "teamindex")
      )
      
      
      # tabItem(
      #   "welcome",
      #   welcomeUI(id = "welcome"),
      #   class = "active"
      # ),
      # tabItem(
      #   "schedule",
      #   titlePanel(
      #     h1("Schedule", align = "center")
      #   ),
      #   scheduleUI(id = "schedule")
      # ),
      # tabItem(
      #   "standings",
      #   titlePanel(
      #     h1("Standings", align = "center")
      #   ),
      #   standingsUI(id = "standings")
      # ),
      # tabItem(
      #   "standingsCup",
      #   titlePanel(
      #     h1("Simulation Soccer Cup", align = "center")
      #   ),
      #   standingsCupUI(id = "standingsCup")
      # ),
      # tabItem(
      #   "teamOverview",
      #   titlePanel(
      #     h1("Team Overview", align = "center")
      #   ),
      #   teamOverviewUI(id = "teamOverview")
      # ),
      # tabItem(
      #   "playerStats",
      #   titlePanel(
      #     h1("Individual Stats", align = "center")
      #   ),
      #   playerStatsUI(id = "playerStats")
      # ),
      # tabItem(
      #   "playerComparison",
      #   titlePanel(
      #     h1("Player Comparison", align = "center")
      #   ),
      #   playerComparisonUI(id = "playerComparison")
      # ),
      # tabItem(
      #   "playerPages",
      #   titlePanel(
      #     h1("Player Pages", align = "center")
      #   ),
      #   playerDatabaseUI(id = "playerPages")
      # ),
      # tabItem(
      #   "trackerPosition",
      #   titlePanel(
      #     h1("Position Tracker", align = "center")
      #   ),
      #   trackerPositionUI(id = "trackerPosition")
      # ),
      # tabItem(
      #   "playerRecords",
      #   titlePanel(
      #     h1("Individual Records", align = "center")
      #   ),
      #   playerRecordsUI(id = "playerRecords")
      # ),
      # tabItem(
      #   "trackerTPE",
      #   titlePanel(
      #     h1("Draft Class Tracker", align = "center")
      #   ),
      #   trackerTPEUI(id = "trackerTPE")
      # ),
      # tabItem(
      #   "regression",
      #   regressionUI(id = "regression")
      # ),
      # tabItem(
      #   "advancedStats",
      #   advancedStatsUI(id = "advancedStats")
      # ),
      # tabItem(
      #   "academyStats",
      #   academyStatsUI(id = "academyStats")
      # ),
      # tabItem(
      #   "fileUpdate",
      #   titlePanel(
      #     h1("File Update Tool", align = "center")
      #   ),
      #   fileUpdateToolUI(id = "fileUpdate")
      # ),
      # tabItem(
      #   "fileCheck",
      #   titlePanel(
      #     h1("File Check Tool", align = "center")
      #   ),
      #   fileCheckUI(id = "fileCheck")
      # ),
      # tabItem(
      #   "playerBuilder",
      #   titlePanel(
      #     h1("Player Builder", align = "center")
      #   ),
      #   hr(),
      #   p(
      #     paste("The Player Builder allows you to build your player",
      #       "using your earned TPE as a bank. The resulting build",
      #       "can then be exported to the Forum using the Export button.")
      #   ),
      #   hr(),
      #   playerBuilderUI(id = "playerBuilder")
      # )
    )
  })
  
  ##---------------------------------------------------------------
  ##                            Sidebar                           -
  ##---------------------------------------------------------------
  output$sidebarpanel <- renderUI({
    
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Your Player",
        tabName = "playerpage",
        selected = TRUE
      ),
      menuItem(
        "Index",
        menuSubItem(
          "League Index",
          tabName = "leagueindex"
        # ),
        # menuSubItem(
        #   "Team Index",
        #   tabName = "teamindex"
        )
      ),
      # menuItem(
      #   "Welcome",
      #   tabName = "welcome",
      #   selected = TRUE
      # ),
      # menuItem(
      #   "SSL Index",
      #   menuSubItem(
      #     "Schedule",
      #     tabName = "schedule"
      #   ),
      #   menuSubItem(
      #     "Standings",
      #     tabName = "standings"
      #   ),
      #   menuSubItem(
      #     "Cup",
      #     tabName = "standingsCup"
      #   ),
      #   menuSubItem(
      #     "Individual Statistics",
      #     tabName = "playerStats"
      #   ),
      #   menuSubItem(
      #     "Advanced Statistics",
      #     tabName = "advancedStats"
      #   ),
      #   menuSubItem(
      #     "Player Records",
      #     tabName = "playerRecords"
      #   )
      # ),
      # menuItem(
      #   "SSL Academy",
      #   menuSubItem(
      #     "Academy Statistics",
      #     tabName = "academyStats"
      #   )
      # ),
      # menuItem(
      #   "Teams",
      #   tabName = "teamOverview"
      # ),
      # menuItem(
      #   "Player Pages",
      #   tabName = "playerPages"
      # ),
      # menuItem(
      #   "Draft Class Tracker",
      #   tabName = "trackerTPE"
      # ),
      # menuItem(
      #   "Player Tools",
      #   # menuSubItem(
      #   #   "Build a new player",
      #   #   tabName = "playerBuilder"
      #   # ),
      #   menuSubItem(
      #     "Player Comparisons",
      #     tabName = "playerComparison"  
      #   ),
      #   menuSubItem(
      #     "Position Tracker",
      #     tabName = "trackerPosition"
      #   ),
      #   menuSubItem(
      #     "Regression",
      #     tabName = "regression"
      #   )
      # ),
      # menuItem(
      #   "Fileworker Tools",
      #   menuSubItem(
      #     "Export Builds",
      #     tabName = "fileUpdate"
      #   ),
      #   menuSubItem(
      #     "Check FM Builds",
      #     tabName = "fileCheck"
      #   )
      # ),
      menuItem(
        "SSL Forum",
        icon = icon("external-link-alt"),
        href = "https://forum.simulationsoccer.com/"
      ),
      menuItem(
        "Github", 
        icon = icon("github"),
        href = "https://github.com/canadice/ssl-index"
      ),
      menuItem(
        "User Profile",
        href = paste("https://forum.simulationsoccer.com/member.php?action=profile&uid=", authOutput()$uid, sep = "")
      )
    )#,
    # extendShinyjs(text = jsResetCode, functions = "restart"), # Add the js code to the page
    # actionButton("reset_button", "Reload Page")
  })
  
  ##----------------------------------------------------------------
  
  ### Sets the url for each tab
  observeEvent(input$tabs,{
    ## Writes a different url based on the tab
    newURL <- paste0(
      session$clientData$url_protocol,
      "//",
      session$clientData$url_hostname,
      ":",
      session$clientData$url_port,
      session$clientData$url_pathname,
      "#",
      input$tabs
    )
    updateQueryString(newURL, mode = "replace", session)
  })
  
  observe({
    currentTab <- sub("#", "", session$clientData$url_hash)
    if(!is.null(currentTab)){
      updateTabItems(session, "tabs", selected = currentTab)
    }
  })
  
  observe({
    req(authOutput()$uid)
    
    ## Loads the different server modules
    playerPageServer("playerpage", userinfo = authOutput())
    leagueIndexServer("leagueindex")
    # teamIndexServer("teamindex")
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "disable")
