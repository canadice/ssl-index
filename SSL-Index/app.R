
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
  require(shinyWidgets, quietly = FALSE)
  
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

shinyDashboardLogoDIY <- 
  function(boldText, 
           mainText, 
           textSize = 15, 
           badgeText, 
           badgeTextColor,
           badgeTextSize = 2, 
           badgeBackColor, 
           badgeBorderRadius = 3) {
    htmlCode <- 
      htmltools::HTML(
        text = paste0("<p style=\"font-size:",
                      textSize, "px\">\n      <b> ", boldText, " </b>", mainText, 
                      "<span> &nbsp; </span>\n      <span style=\"background-color: ", 
                      badgeBackColor, ";\n      border-radius: ", badgeBorderRadius, 
                      "px; \"> &nbsp;\n      <font color=\"", badgeTextColor, 
                      "\" size=\"", badgeTextSize, "\">", badgeText, "  </font> &nbsp; </span> </p>")
        )
    htmlCode <- gsub(pattern = "\n", replacement = "", x = htmlCode)
    htmlCode <- gsub(pattern = "[[:space:]]{2,3}", replacement = "", 
                     x = htmlCode)
    return(htmlCode)
}


##################################################################
##                      SSL Logo and Theme                      ##
##################################################################

# sslBlueD <- "#070B51"
# sslBlueL <- "#141204"
# sslGold <- "#BD9523"

sslBlueL <- "#324f7e"
sslBlueD <- "#4b8dad"
sslGold <- "#BD9523"

customLogo <- 
  shinyDashboardLogoDIY(
    boldText = "",
    mainText = tags$a(
      href='https://forum.simulationsoccer.com',
      target="_blank",
      tags$img(src='portallogo.png', height = "70"),
    ),
    badgeText = "v0.8",
    badgeTextColor = "white",
    badgeBackColor = sslBlueL
  )

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
    title = "SSL Portal",
    dashboardHeader(
      title = customLogo,
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
          ),
          # 
          # ## Changes the margin of the sidebar
          # tags$style(".main-header .navbar {margin-left: 300px}"),
          # tags$style(type="text/css", "text {font-family: sans-serif, courier}"),
          # 
        ),
        dropdownMenuOutput("notifications")
      )
    ),
    dashboardSidebar(
      # Adjust the sidebar in accordance with the higher header
      tags$style(".left-side, .main-sidebar {padding-top: 100px}"),
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
        welcomeUI(id = "welcome"),
        active = TRUE
      ),
      tabItem(
        "bodoverview",
        bodTeamUI(id = "bodoverview")
      ),
      tabItem(
        "assignManager",
        assignManagerUI("assignManager")
      ),
      tabItem(
        "submitPT",
        submitPTUI(id = "submitPT")
      ),
      tabItem(
        "exportBuild",
        exportBuildUI(id = "exportBuild")
      # ),
      # tabItem(
      #   "bodoverview",
      #   bodTeamUI(id = "bodoverview")
      # ),
      # tabItem(
      #   "teamindex",
      #   teamIndexUI(id = "teamindex")
      )
      
    )
  })
  
  output$notifications <- renderMenu({
    if(any(
      getPlayersForApproval() %>% nrow() > 0
    )){
      if(any(c(3, 4, 15) %in% authOutput()$usergroup)){
        dropdownMenu(type = "notifications",
                     badgeStatus = "warning",
                     notificationItem(
                       text = paste(getPlayersForApproval() %>% nrow(), "new players awaiting approval."),
                       icon("users"),
                       href = "/#playerapprove"
                     )
        )
      } else {
        dropdownMenu(type = "notifications",
                     badgeStatus = "warning",
                     notificationItem(
                       text = "Placeholder",
                       icon("truck"),
                       status = "success"
                     )
        )
      }
    }
  }) %>% 
    bindEvent(input$tabs, ignoreInit = FALSE)
  
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
      # menuItem(
      #   "Index",
      #   menuSubItem(
      #     "League Index",
      #     tabName = "leagueindex"
      #   # ),
      #   # menuSubItem(
      #   #   "Team Index",
      #   #   tabName = "teamindex"
      #   )
      # ),
      {
        # PT (11), Commissioner (4)
        if(any(c(4, 11) %in% authOutput()$usergroup)){
          menuItem(
            "PT Tools",
            menuSubItem(
              "Submit Graded PT",
              tabName = "submitPT"
            )
          )
        }
      },
      {
        # Fileworker (14), Commissioner (4)
        if(any(c(4, 3, 14) %in% authOutput()$usergroup)){
          menuItem(
            "File Work Tools",
            menuSubItem(
              "Export Builds",
              tabName = "exportBuild"
            )
          )
        }
      },
      {
        # Manager (8)
        if(any(c(4, 8) %in% authOutput()$usergroup)){
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
              "Assign Managers",
              tabName = "assignManager"
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
  
  observe({
    updateTabItems(session, "tabs", selected = "welcome")
  }) %>% 
    bindEvent(
      authOutput()
    )
  
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
    leagueIndexServer("leagueindex", userinfo = authOutput())
    
    managerTeamServer("managerteam", userinfo = authOutput())
    assignManagerServer("assignManager", userinfo = authOutput())
    bodTeamServer("bodoverview", userinfo = authOutput())
    exportBuildServer("exportBuild")
    
    playerPageServer("playerpage", uid = authOutput()$uid, parent = session)
    createPlayerServer("createplayer", userinfo = authOutput(), parent = session)
    
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
    } else if(input$tabs == "submitPT"){
      req(authOutput()$uid)
      submitPTServer("submitPT", userinfo = authOutput())
    } else if(input$tabs == "welcome"){
      req(authOutput()$uid)
      welcomeServer("welcome", userinfo = authOutput())
    }
  }) %>% 
    bindEvent(
      input$tabs
    )
  
}
# Run the application 
app <- shinyApp(ui = ui, server = server, enableBookmarking = "disable")
