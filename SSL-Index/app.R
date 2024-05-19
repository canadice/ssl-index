
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
  require(shinyFeedback, quietly = FALSE)
  require(sortable, quietly = FALSE)
  
  require(fresh, quietly = FALSE)
  require(shiny.router, quietly = FALSE)
  
  require(vembedr, quietly = FALSE)
  
  ## Packages for asynchronuous programming
  require(promises, quietly = FALSE)
  require(future, quietly = FALSE)
  
  ## Package for login
  require(sodium, quietly = FALSE)
  require(shinymanager, quietly = FALSE)
})

## Sets up that evaluating futures is done in parallell
plan(multisession)


##################################################################
##                      SSL Logo and Theme                      ##
##################################################################

# sslBlueD <- "#070B51"
# sslBlueL <- "#141204"
# sslGold <- "#BD9523"

sslBlueL <- "#324f7e"
sslBlueD <- "#4b8dad"
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
      box_bg = "#e4eef3", 
      info_box_bg = "#e4eef3"
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
      title = "TESTING GROUND",
      tags$li(
        tags$head(
          tags$link(
            rel = "icon", 
            type = "image/png", 
            href = "favicon.png"),
          tags$title("TESTING GROUND")
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
      useShinyFeedback(), # include shinyFeedback
      useShinyjs(),
      uiOutput("body")
    )
  )
}

## Adds shinymanager authentication to the app
ui <- 
  secure_app(
    ui,
    fab_position = "bottom-left"
  )

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
        playerPageUI(id = "playerpage")
      ),
      tabItem(
        "leagueindex",
        leagueIndexUI(id = "leagueindex")
      ),
      tabItem(
        "createplayer",
        createPlayerUI(id = "createplayer")
      ),
      tabItem(
        "playerapprove",
        playerApproveUI(id = "playerapprove")
      ),
      tabItem(
        "teamView",
        managerTeamUI(id = "managerteam")
      ),
      tabItem(
        "welcome",
        managerTeamUI(id = "teamindex")
      ),
      tabItem(
        "bodoverview",
        bodTeamUI(id = "bodoverview")
      # ),
      # tabItem(
      #   "teamindex",
      #   teamIndexUI(id = "teamindex")
      )
      
    )
  })
  
  ##---------------------------------------------------------------
  ##                            Sidebar                           -
  ##---------------------------------------------------------------
  output$sidebarpanel <- renderUI({
    
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Welcome",
        tabName = "welcome",
        selected = TRUE
      ),
      menuItemOutput("playerTabs"),
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
      {
        # Manager (8)
        if(8 %in% authOutput()$usergroup){
          menuItem(
            "Manager Tools",
            menuSubItem(
              "Your Team",
              tabName = "teamView"
              # ),
              # menuSubItem(
              #   "Team Index",
              #   tabName = "teamindex"
            )
          )
        }
      },
      {
        # BoD (3), Commissioner (4) or Intern (15)
        if(any(c(3,4, 15) %in% authOutput()$usergroup)){
          menuItem(
            "BoD Tools",
            menuSubItem(
              "Player Approvals",
              tabName = "playerapprove"
            ),
            menuSubItem(
              "Organizational Overview",
              tabName = "bodoverview"
            )
          )
        }
      },
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
        "Your User",
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
  
  output$playerTabs <- renderMenu({
    if(hasActivePlayer(authOutput()$uid)){
      menuItem(
        "Your Player",
        tabName = "playerpage"
      )
    } else if(checkIfAlreadyApproving(authOutput()$uid)) {
      menuItem(
        "Your player is awaiting approval"
      )
    } else {
      menuItem(
        "Create",
        tabName = "createplayer"
      )
    }
  }) %>% 
    bindEvent(
      input$tabs,
      ignoreInit = FALSE,
      ignoreNULL = FALSE
    )
  
  observe({
    req(authOutput()$uid)
    
    # print("Authenticating")
    # print(authOutput())
    # 
    ## Loads the different server modules
    playerPageServer("playerpage", uid = authOutput()$uid)
    leagueIndexServer("leagueindex", userinfo = authOutput())
    createPlayerServer("createplayer", userinfo = authOutput(), parent = session)
    managerTeamServer("managerteam", userinfo = authOutput())
    bodTeamServer("bodoverview", userinfo = authOutput())
    # teamIndexServer("teamindex")
  }) %>% 
    bindEvent(
      authOutput(),
      ignoreInit = FALSE
    )
  
  observe({
    if(input$tabs == "playerapprove"){
      req(authOutput()$uid)
      playerApproveServer("playerapprove", userinfo = authOutput())  
    } 
  }) %>% 
    bindEvent(
      input$tabs
    )
  
}
# Run the application 
app <- shinyApp(ui = ui, server = server, enableBookmarking = "disable")
