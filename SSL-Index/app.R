
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
  require(rvest, quietly = FALSE)
  require(scales, quietly = FALSE)
  
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
  # require(bslib, quietly = FALSE) ## Incompatible with shinydashboard
  
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
      tags$img(src='portalwhite.png', height = "70")
    ),
    badgeText = "v0.9.1",
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
        div(
          class = "navbarHead",
          tags$p(actionButton(inputId = "gotoportal",
                            label = "Portal")),
          tags$p(actionButton(inputId = "gotoindex",
                            label = "Index"))
        ),
        dropdownMenuOutput("notifications")
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
    status = "primary",
    tags_top = 
      list(
        tags$div(
          tags$h4("SSL Portal", style = "align:center"),
          tags$img(
            src = "FA.png", width = 100
          )
        ),
        tags$style(
          type="text/css",
          "body {font-family: 'Gotham SSm A', 'Gotham SSm B', Helvetica, sans-serif;}
          h1, h2, h3, h4, h5 {
            font-family: 'Gotham SSm A', 'Gotham SSm B', Helvetica, sans-serif;
            font-weight: 800; font-style: normal;
          }"
        )
      ),
    tags_bottom = tags$div(
      tags$a("Register a new user!", href = "https://forum.simulationsoccer.com/member.php?action=register", target = "_blank", style = "float: left;"),
      tags$a("Forgot password?", href = "https://forum.simulationsoccer.com/member.php?action=lostpw", target = "_blank", style = "float:right;"),
      tags$br(),
      div(
        align = "center",
        tags$p(actionButton(inputId = "login_guest",
                          label = "Continue as guest"))
      )
    ),
    fab_position = "bottom-left"
  )

server <- function(input, output, session) {
  
  resAuth <- secure_server(
    check_credentials = customCheckCredentials(),
    session = session
  )
  
  # login as guest
  observe({
    token <- shinymanager:::.tok$generate("guest")
    shinymanager:::.tok$add(token, list(user = "guest", role = "guest", usergroup = 0))
    shinymanager:::addAuthToQuery(session, token, "en")
    session$reload()
  }) %>% 
    bindEvent(
      input$login_guest
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
      ),
      tabItem(
        "uploadGame",
        uploadGameUI(id = "uploadGame")
      ),
      tabItem(
        "bankOverview",
        bankOverviewUI(id = "bankOverview")
      ),
      tabItem(
        "bankDeposit",
        bankDepositUI(id = "bankDeposit")
      ),
      tabItem(
        "bankProcess",
        bankProcessUI(id = "bankProcess")
      # ),
      # tabItem(
      #   "teamindex",
      #   teamIndexUI(id = "teamindex")
      # ),
      # tabItem(
      #   "teamindex",
      #   teamIndexUI(id = "teamindex")
      # ),
      # tabItem(
      #   "teamindex",
      #   teamIndexUI(id = "teamindex")
      # ),
      # tabItem(
      #   "teamindex",
      #   teamIndexUI(id = "teamindex")
      # ),
      # tabItem(
      #   "teamindex",
      #   teamIndexUI(id = "teamindex")
      # ),
      # tabItem(
      #   "teamindex",
      #   teamIndexUI(id = "teamindex")
      # ),
      # tabItem(
      #   "teamindex",
      #   teamIndexUI(id = "teamindex")
      )
      
    )
  })
  
  output$notifications <- renderMenu({
    if(any(c(3, 4, 15) %in% authOutput()$usergroup)){
      if(any(
        getPlayersForApproval() %>% nrow() > 0
      )){
        dropdownMenu(type = "notifications",
                     badgeStatus = "warning",
                     notificationItem(
                       text = paste(getPlayersForApproval() %>% nrow(), "new players awaiting approval."),
                       icon("users")
                     )
        )
      }
    } else {
      
    }
  }) %>% 
    bindEvent(input$tabs, ignoreInit = FALSE)
  
  ##---------------------------------------------------------------
  ##                            Sidebar                           -
  ##---------------------------------------------------------------
  menuGroup <- reactiveVal({0})
  
  observe({
    menuGroup(1)
    if(!(input$tabs %>% is.null())){
      if(input$tabs != "welcome"){
        updateTabItems(session, "tabs", "welcome")  
      }  
    }
  }) %>% 
    bindEvent(
      input$gotoindex
    )
  
  observe({
    menuGroup(0)
    if(!(input$tabs %>% is.null())){
      if(input$tabs != "welcome"){
        updateTabItems(session, "tabs", "welcome")  
      }  
    }
  }) %>% 
    bindEvent(
      input$gotoportal
    )
  
  output$sidebarpanel <- renderUI({
    if(menuGroup() %% 2 == 0){
      sidebarMenu(
        id = "tabs",
        menuItem(
          "Welcome",
          tabName = "welcome",
          selected = TRUE
        ),
        {
          if(!any(0 %in% authOutput()$usergroup)){
            tagList(
              menuItemOutput("playerTabs"),
              menuItem(
                "SSL Bank",
                menuSubItem(
                  "Player Store",
                  tabName = "bankOverview"
                ),
                {
                  if(any(c(3, 4, 8, 11, 12) %in% authOutput()$usergroup)){
                    menuSubItem(
                      "Bank Deposits",
                      tabName = "bankDeposit"
                    )
                  }
                },
                {
                  # Banker (12), BoD (3), Commissioner (4)
                  if(any(c(3, 4, 12) %in% authOutput()$usergroup)){
                    menuSubItem(
                      "Process Transactions",
                      tabName = "bankProcess"
                    )
                  }
                }
              ),
              {
                # PT (11), Commissioner (4)
                if(any(c(3, 4, 11) %in% authOutput()$usergroup)){
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
                    ),
                    menuSubItem(
                      "Upload Game Stats",
                      tabName = "uploadGame"
                    )
                  )
                }
              },
              {
                # Manager (8)
                if(any(c(3, 4, 8) %in% authOutput()$usergroup)){
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
                if(any(c(3, 4, 15) %in% authOutput()$usergroup)){
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
              }
            )
          }
        },
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
          if(!any(0 %in% authOutput()$usergroup)){
            menuItem(
              "Your User",
              href = paste("https://forum.simulationsoccer.com/member.php?action=profile&uid=", authOutput()$uid, sep = "")
            )
          } else {
            menuItem(
              "Register a user",
              href = paste("https://forum.simulationsoccer.com/member.php?action=register")
            )
          }
        },
        menuItem(
          "SSL Forum",
          icon = icon("external-link-alt"),
          href = "https://forum.simulationsoccer.com/"
        ),
        div(class = "stickyFooter",
            tags$a("Made by Canadice", href = "https://github.com/canadice/ssl-index", target = "_blank"))
      )
    } else {
      sidebarMenu(
        id = "tabs",
        menuItem(
          "Welcome",
          tabName = "welcome",
          selected = TRUE
        ),
        {
          menuItem(
            "League Index",
            tabName = "leagueindex"
          )
        },
        {
          if(!any(0 %in% authOutput()$usergroup)){
            menuItem(
              "Your User",
              href = paste("https://forum.simulationsoccer.com/member.php?action=profile&uid=", authOutput()$uid, sep = "")
            )
          } else {
            menuItem(
              "Register a user",
              href = paste("https://forum.simulationsoccer.com/member.php?action=register")
            )
          }
        },
        menuItem(
          "SSL Forum",
          icon = icon("external-link-alt"),
          href = "https://forum.simulationsoccer.com/"
        ),
        div(class = "stickyFooter",
            tags$a("Made by Canadice", href = "https://github.com/canadice/ssl-index", target = "_blank"))
      )
    }
    #,
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
  
  # Goes to welcome tab after logging in successfully
  observe({
    updateTabItems(session, "tabs", selected = "welcome")
  }) %>% 
    bindEvent(
      authOutput()
    )
  
  # Different player menu based on if you have a current player or not
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
        "Create a player",
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
    managerTeamServer("managerteam", userinfo = authOutput())
    assignManagerServer("assignManager", userinfo = authOutput())
    bodTeamServer("bodoverview", userinfo = authOutput())
    exportBuildServer("exportBuild")
    
  }) %>% 
    bindEvent(
      authOutput(),
      ignoreInit = FALSE
    )
  
  loadedPage <- 
    reactiveValues(
      create = FALSE,
      player = FALSE,
      index = FALSE,
      uploadGame = FALSE,
      bankOverview = FALSE
    )
  
  ## Adds a reactive value to send to player page and bank overview that will trigger a reload of player data in case something happens in either
  updated <- reactiveVal({0})
  
  observe({
    if(input$tabs == "playerapprove"){
      req(authOutput()$uid)
      playerApproveServer("playerapprove", userinfo = authOutput())
      
    } else if(input$tabs == "submitPT"){
      req(authOutput()$uid)
      submitPTServer("submitPT", userinfo = authOutput())
      
    } else if(input$tabs == "welcome"){
      welcomeServer("welcome")
      
    } else if(input$tabs == "bankDeposit"){
      req(authOutput()$uid)
      bankDepositServer("bankDeposit", userinfo = authOutput())
      
    } else if(input$tabs == "bankProcess"){
      req(authOutput()$uid)
      bankProcessServer("bankProcess", userinfo = authOutput())
      
    } else if(!loadedPage$create & input$tabs == "createplayer"){
      req(authOutput()$uid)
      createPlayerServer("createplayer", userinfo = authOutput(), parent = session)
      loadedPage$create <- TRUE
      
    } else if(!loadedPage$player & input$tabs == "playerpage"){
      req(authOutput()$uid)
      playerPageServer("playerpage", uid = authOutput()$uid, parent = session, updated = updated)
      loadedPage$player <- TRUE
      
    } else if(!loadedPage$index & input$tabs == "leagueindex"){
      leagueIndexServer("leagueindex")
      loadedPage$index <- TRUE
      
    } else if(!loadedPage$uploadGame & input$tabs == "uploadGame"){
      uploadGameServer("uploadGame")
      loadedPage$uploadGame <- TRUE
      
    } else if(!loadedPage$bankOverview & input$tabs == "bankOverview"){
      bankOverviewServer("bankOverview", uid = authOutput()$uid, parent = session, updated = updated)
      loadedPage$bankOverview <- TRUE
      
    }
    
  }) %>% 
    bindEvent(
      input$tabs
    )
  
}
# Run the application 
app <- shinyApp(ui = ui, server = server, enableBookmarking = "disable")
