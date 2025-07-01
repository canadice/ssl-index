
###########################################################################
###########################################################################
###                                                                     ###
###                              SSL INDEX                              ###
###                                                                     ###
###########################################################################
###########################################################################

version <- "v2.3"

suppressMessages({
  ## Data handling
  require(dplyr, quietly = FALSE)
  require(tidyr, quietly = FALSE)
  require(purrr, quietly = FALSE)
  require(arsenal, quietly = FALSE)
  require(rvest, quietly = FALSE)
  require(scales, quietly = FALSE)
  require(rlang, quietly = FALSE)
  
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
  require(highcharter, quietly = FALSE)
  
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
  
  ## Package for int overflow
  require(bit64, quietly = FALSE)
  
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
  
  require(googlesheets4, quietly = FALSE)
  
  gs4_deauth()
  
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
    badgeText = version,
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
        ## Function that loads js-cookies for auto-login
        tags$script(
          src = paste0(
            "https://cdn.jsdelivr.net/npm/js-cookie@rc/",
            "dist/js.cookie.min.js"
          )
        ),
        tags$script("Shiny.addCustomMessageHandler('cookie-remove', function(msg){
                        Cookies.remove(msg.name);
                        getCookies();
                      })
                    "),
        tags$head(
          tags$link(
            rel = "icon", 
            type = "image/png", 
            href = "favicon.png"),
          tags$title("SSL Portal")
        ),
        class = "dropdown"
      )
    ),
    dashboardSidebar(
      uiOutput("sidebarpanel") %>% 
        withSpinnerSmall()
    ),
    dashboardBody(
      customTheme %>% use_theme(),
      includeCSS('style.css'),
      useShinyFeedback(), # include shinyFeedback
      useShinyjs(), # include shinyjs
      uiOutput("body"),

      # User menu FAB
      # Wasn't able to fully customize the Rshiny FAB, so I created a new one
      tags$div(
        class = "homemade-user-fab",
        icon(
          "user",
          class = "fa-solid",
          style = "color: black; font-size: 28px; padding: 8px;",
          # Support showing and hiding user options on mobile
          ontouchstart = "
            var buttons = document.querySelector('.fab-action-buttons');
            var isShowing = getComputedStyle(buttons).opacity === '1';
            buttons.style.opacity = isShowing ? 0 : 1;
            buttons.style.visibility = isShowing ? 'hidden' : 'visible';
          "
        ),
        role = "button",
        div(
          class = "fab-action-buttons",
          tagList(
            flexCol(
              style = "gap: 4px;",
              tagList(
                # User menu items. Can be extended by adding more actionButtons.
                # Just remember to connect any button's click event to an action server-side.
                actionButton(
                  inputId = "player",
                  label = "Your Player",
                  icon = icon("futbol"),
                  class = "centered-flex-content",
                  style = "justify-content: flex-start; gap: 8px;"
                ),
                actionButton(
                  inputId = "userbank",
                  label = "Bank/Store",
                  icon = icon("building-columns"),
                  class = "centered-flex-content",
                  style = "justify-content: flex-start; gap: 8px;"
                ),
                actionButton(
                  inputId = "logout",
                  label = "Logout",
                  icon = icon("door-open"),
                  class = "centered-flex-content",
                  style = "justify-content: flex-start; gap: 8px;"
                )
              )
            )
          )
        )
      )
    )
  )
}

## Adds shinymanager authentication to the app
ui <- 
  secure_app(
    ui,
    lan = "en",
    status = "primary",
    ## Login page functionality
    tags_top = 
      list(
        ## js function for storing cookies
        tags$script(
          src = paste0(
            "https://cdn.jsdelivr.net/npm/js-cookie@rc/",
            "dist/js.cookie.min.js"
          )
        ),
        tags$script("// script.js
                      function getCookies(){
                        var res = Cookies.get();
                        Shiny.setInputValue('cookies', res);
                      }
                    
                    // script.js
                      Shiny.addCustomMessageHandler('cookie-set', function(msg){
                        Cookies.set(msg.name, msg.value);
                        getCookies();
                      })
                      
                      Shiny.addCustomMessageHandler('cookie-remove', function(msg){
                        Cookies.remove(msg.name);
                        getCookies();
                      })
                    
                    $(document).on('shiny:connected', function(ev){
                      getCookies();
                    });"),
        tags$div(
          tags$h4("SSL Portal", style = "align:center"),
          tags$img(src = "FA.png", width = 100)
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
    fab_position = "none"
  )
  
server <- function(input, output, session) {
  
  resAuth <- secure_server(
    check_credentials = customCheckCredentials(),
    timeout = 45,
    session = session
  )
  
  # login as guest
  observe({
    token <- shinymanager:::.tok$generate("guest")
    shinymanager:::.tok$add(token, list(user = "guest", role = "guest", usergroup = 0))
    shinymanager:::addAuthToQuery(session, token, "en")
    session$reload()
  }) %>% 
    bindEvent(input$login_guest)
  
  # Checks saved cookie for automatic login
  observe({
    refreshtoken <- getRefreshToken(input$cookies$token)
    if(refreshtoken %>% nrow() > 0){
      if((now() %>% as.numeric()) < refreshtoken$expires_at){
        token <- shinymanager:::.tok$generate(refreshtoken$username)
        shinymanager:::.tok$add(token, list(
          uid = refreshtoken$uid, 
          username = refreshtoken$username, 
          usergroup = 
            paste(refreshtoken$usergroup, refreshtoken$additionalgroups, sep = ",") %>% 
            str_split(pattern = ",", simplify = TRUE) %>%
            as.numeric() %>% 
            as.list(),
          suspended = refreshtoken$suspendposting == 1
        ))
        shinymanager:::addAuthToQuery(session, token, "en")
        
        setRefreshToken(uid = refreshtoken$uid, token = refreshtoken$token)
        
        session$reload()
      }
    }
  }) %>% 
    bindEvent(input$cookies$token, ignoreNULL = TRUE)
  
  ## Removes cookie when logging out
  observe({
    msg <- list(name = "token")
    session$sendCustomMessage("cookie-remove", msg)
  }) %>% 
    bindEvent(input$logout)
  
  ## Adds all authentication list to a reactive object
  authOutput <- reactive({
    reactiveValuesToList(resAuth)
  })
  
  #### BODY ####
  output$body <- renderUI({
    tabItems(
      tabItem("yourPlayer",yourPlayerUI(id = "yourPlayer")),
      tabItem("leagueindex",leagueIndexUI(id = "leagueindex")),
      tabItem("createplayer",createPlayerUI(id = "createplayer")),
      tabItem("playerapprove",playerApproveUI(id = "playerapprove")),
      tabItem("managerTeam",managerTeamUI(id = "managerTeam")),
      tabItem("welcome",welcomeUI(id = "welcome"),active = TRUE),
      tabItem("bodoverview",bodTeamUI(id = "bodoverview")),
      tabItem("assignManager",assignManagerUI("assignManager")),
      tabItem("submitPT",submitPTUI(id = "submitPT")),
      tabItem("exportBuild",exportBuildUI(id = "exportBuild")),
      tabItem("uploadGame",uploadGameUI(id = "uploadGame")),
      tabItem("bankOverview",bankOverviewUI(id = "bankOverview")),
      tabItem("bankDeposit",bankDepositUI(id = "bankDeposit")),
      tabItem("bankProcess",bankProcessUI(id = "bankProcess")),
      tabItem("editSchedule",editScheduleUI(id = "editSchedule")),
      tabItem("academyUpload",academyUploadUI(id = "academyUpload")),
      tabItem("academyIndex",academyIndexUI(id = "academyIndex")),
      tabItem("careerRecords",careerRecordsUI(id = "careerRecords")),
      tabItem("playerPages",playerPagesUI(id = "playerPages")),
      tabItem("playerEdit",playerEditUI(id = "playerEdit")),
      tabItem("leagueStandings",leagueStandingsUI(id = "leagueStandings")),
      tabItem("draftClass",draftClassUI(id = "draftClass")),
      tabItem("organizationPages",organizationPagesUI(id = "organizationPages")),
      tabItem("leagueSchedule",leagueScheduleUI(id = "leagueSchedule")),
      tabItem("nationTracker",nationTrackerUI(id = "nationTracker")),
      tabItem("positionTracker",positionTrackerUI(id = "positionTracker")),
      tabItem("academyStandings",academyStandingsUI(id = "academyStandings"))
      
      # tabItem("contractProcess",contractProcessUI(id = "contractProcess")),
      # tabItem("budgetOverview",budgetOverviewUI(id = "budgetOverview")),
      # tabItem("tradeProcess",tradeProcessUI(id = "tradeProcess")),
      # tabItem("teamindex",teamIndexUI(id = "teamindex")),
    )
  })


  #### User FAB ###

  # Hides the user FAB if visitor isn't logged in
  observe({
    if (any(c(0,5) %in% authOutput()$usergroup)) {
      removeUI(
        selector = ".homemade-user-fab"
      )
    }
  }) %>%
    bindEvent(authOutput())

  observeEvent(input$player, {
    updateTabItems(session, "tabs", "yourPlayer")
  })

  observeEvent(input$userbank, {
    updateTabItems(session, "tabs", "bankOverview")
  })

  observeEvent(input$logout, {
    session$reload()
  })
  
  #### SIDEBAR ####
  
  ## Navigation between Portal and Index
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
  
  ## Rendered menu in sidebar
  output$sidebarpanel <- renderUI({
    
    # print(!authOutput()$suspended)
    if(menuGroup() %% 2 == 0){
      #### PORTAL SIDEBAR MENU ####
      tagList(
        tags$div(
          class = "navbarHead",
          tags$p(
            actionButton(
              inputId = "gotoportal",
              label = "Portal",
              style = "background: #BD9523;"
            )
          ),
          tags$p(
            actionButton(
              inputId = "gotoindex",
              label = "Index"
            )
          )
        ),
        sidebarMenu(
          id = "tabs",
          menuItem("Welcome",tabName = "welcome",selected = TRUE),
          {
            if(!any(c(0,5, 7) %in% authOutput()$usergroup) & !authOutput()$suspended){
              tagList(
                menuItemOutput("playerTabs"),
                
                ### BANK MENU
                menuItem("SSL Bank",
                         {
                           if(hasActivePlayer(authOutput()$uid)){
                             menuSubItem("Player Store",tabName = "bankOverview")
                           }
                         },
                         {
                           # BoD, Commissioner, Manager, PT Team and Banker
                           if(any(c(3, 4, 8, 11, 12) %in% authOutput()$usergroup)){
                             menuSubItem("Bank Deposits",tabName = "bankDeposit")
                           }
                         },
                         {
                           # Banker (12), BoD (3), Commissioner (4)
                           if(any(c(3, 4, 12) %in% authOutput()$usergroup)){
                             menuSubItem("Process Bank Transactions",tabName = "bankProcess")
                           }
                         }
                ),
                menuItem("Players and Organizations",
                         menuSubItem("Player Pages",tabName = "playerPages"),
                         menuSubItem("Organization Pages",tabName = "organizationPages")),
                menuItem("Trackers",
                         menuSubItem("Draft Class Tracker",tabName = "draftClass"),
                         menuSubItem("Position Tracker", tabName = "positionTracker"),
                         menuSubItem("WSFC Nation Tracker", tabName = "nationTracker")),
                if(!any(c(2) %in% authOutput()$usergroup)){hr()},
                # menuItem(
                #   "SSL Budget",
                #   menuSubItem(
                #     "Budget Overview",
                #     tabName = "budgetOverview"
                #   ),
                #   {
                #     # Banker (12), BoD (3), Commissioner (4)
                #     if(any(c(3, 4, 12) %in% authOutput()$usergroup)){
                #       tagList(
                #         menuSubItem(
                #           "Process Contracts",
                #           tabName = "contractProcess"
                #         ),
                #         menuSubItem(
                #           "Process Trade",
                #           tabName = "tradeProcess"
                #         )
                #       )
                #     }
                #   }
                # ),
                ### PT TOOLS
                {
                  # PT (11), Commissioner (4)
                  if(any(c(3, 4, 11) %in% authOutput()$usergroup)){
                    menuItem("PT Tools",
                             menuSubItem("Submit Graded PT",tabName = "submitPT")
                    )
                  }
                },
                
                ### FILEWORKER TOOLS
                {
                  # Fileworker (14), Commissioner (4)
                  if(any(c(4, 3, 14) %in% authOutput()$usergroup)){
                    menuItem(
                      "File Work Tools",
                      menuSubItem("Export Builds",tabName = "exportBuild"),
                      menuSubItem("Upload Game Stats",tabName = "uploadGame"),
                      menuSubItem("Upload Academy Stats",tabName = "academyUpload"),
                      menuSubItem("Edit Schedule",tabName = "editSchedule")
                    )
                  }
                },
                
                ### MANAGER TOOLS
                {
                  # Manager (8)
                  if(any(c(3, 4, 8) %in% authOutput()$usergroup)){
                    menuItem(
                      "Manager Tools",
                      menuSubItem("Your Team",tabName = "managerTeam")
                    )
                  }
                },
                
                ### BOD TOOLS
                {
                  # BoD (3), Commissioner (4) or Intern (15)
                  if(any(c(3, 4, 15) %in% authOutput()$usergroup)){
                    menuItem(
                      "BoD Tools",
                      menuSubItem("Player Approvals",tabName = "playerapprove"),
                      menuSubItem("Edit Player",tabName = "playerEdit"),
                      menuSubItem("Assign Managers",tabName = "assignManager"),
                      menuSubItem("Organizational Overview",tabName = "bodoverview"),
                      menuSubItem("Edit Schedule",tabName = "editSchedule")
                    )
                  }
                }
              )
            }
          },
          hr(),
          {
            if(!any(c(0,5) %in% authOutput()$usergroup)){
              menuItem("Your User",href = paste("https://forum.simulationsoccer.com/member.php?action=profile&uid=", authOutput()$uid, sep = ""))
            } else {
              menuItem("Register a user",href = paste("https://forum.simulationsoccer.com/member.php?action=register"))
            }
          },
          menuItem("SSL Forum",icon = icon("external-link-alt"),href = "https://forum.simulationsoccer.com/"),
          div(class = "stickyFooter",
              tags$a("Made by Canadice", href = "https://github.com/canadice/ssl-index", target = "_blank"))
        )
      )
    } else {
      #### INDEX SIDEBAR MENU ####
      sidebarMenu(
        id = "tabs",
        tags$div(
          class = "navbarHead",
          tags$p(
            actionButton(
              inputId = "gotoportal",
              label = "Portal"
            )
          ),
          tags$p(
            actionButton(
              inputId = "gotoindex",
              label = "Index",
              style = "background: #BD9523;"
            )
          )
        ),
        menuItem("Welcome",tabName = "welcome",selected = TRUE),
        menuItem("Academy Index",tabName = "academyIndex"),
        menuItem("Academy Standings", tabName = "academyStandings"),
        menuItem("League Index",tabName = "leagueindex"),
        menuItem("Standings", tabName = "leagueStandings"),
        menuItem("Schedule", tabName = "leagueSchedule"),
        menuItem("Career Records",tabName = "careerRecords"),
        {
          if(!any(0 %in% authOutput()$usergroup)){
            menuItem("Your User",href = paste("https://forum.simulationsoccer.com/member.php?action=profile&uid=", authOutput()$uid, sep = ""))
          } else {
            menuItem("Register a user",href = paste("https://forum.simulationsoccer.com/member.php?action=register"))
          }
        },
        menuItem("SSL Forum",icon = icon("external-link-alt"),href = "https://forum.simulationsoccer.com/"),
        div(class = "stickyFooter",
            tags$a("Made by Canadice", href = "https://github.com/canadice/ssl-index", target = "_blank"))
      )
    }
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
    bindEvent(authOutput(),once = TRUE)
  
  # Different player menu based on if you have a current player or not
  output$playerTabs <- renderMenu({
    req(authOutput())
    
    if(hasActivePlayer(authOutput()$uid)){
      menuItem("Your Player",tabName = "yourPlayer")
    } else if(checkIfAlreadyApproving(authOutput()$uid)) {
      menuItem("Your player is awaiting approval")
    } else {
      menuItem("Create a player",tabName = "createplayer")
    }
  }) %>% 
    bindEvent(input$tabs,ignoreInit = FALSE,ignoreNULL = FALSE)
  
  observe({
    req(authOutput()$uid)
    
    # print("Authenticating")
    # print(authOutput())
    # 
    ## Loads the different server modules
    
    
  }) %>% 
    bindEvent(authOutput(),ignoreInit = FALSE)
  
  loadedPage <- 
    reactiveValues(
      create = FALSE, player = FALSE, index = FALSE, academyIndex = FALSE, uploadGame = FALSE,
      bankOverview = FALSE, welcome = FALSE, records = FALSE, playerPages = FALSE, contractProcess = FALSE,
      tradeProcess = FALSE, playerEdit = FALSE, submitPT = FALSE, bankDeposit = FALSE, bankProcess = FALSE,
      leagueStandings = FALSE, leagueSchedule = FALSE, managerTeam = FALSE, assignManager = FALSE,
      bodoverview = FALSE, exportBuild = FALSE, organizationPages = FALSE, draftClass = FALSE,
      nationTracker = FALSE, positionTracker = FALSE, academyStandings = FALSE
    )
  
  ## Adds a reactive value to send to player page and bank overview that will trigger a reload of player data in case something happens in either
  updated <- reactiveVal({0})
  
  observe({
    if(input$tabs == "playerapprove"){
      req(authOutput()$uid)
      playerApproveServer("playerapprove", userinfo = authOutput())
      
    } else if(!loadedPage$submitPT & input$tabs == "submitPT"){
      req(authOutput()$uid)
      submitPTServer("submitPT", userinfo = authOutput())
      loadedPage$submitPT <- TRUE
      
    } else if(!loadedPage$welcome & input$tabs == "welcome"){
      welcomeServer("welcome", usergroup = authOutput()$usergroup)
      loadedPage$welcome <- TRUE
      
    } else if(!loadedPage$bankDeposit & input$tabs == "bankDeposit"){
      req(authOutput()$uid)
      bankDepositServer("bankDeposit", userinfo = authOutput())
      loadedPage$bankDeposit <- TRUE
      
    } else if(!loadedPage$bankProcess & input$tabs == "bankProcess"){
      req(authOutput()$uid)
      bankProcessServer("bankProcess", userinfo = authOutput())
      loadedPage$bankProcess <- TRUE
      
    } else if(input$tabs == "editSchedule"){
      req(authOutput()$uid)
      editScheduleServer("editSchedule")
      
    } else if(input$tabs == "academyUpload"){
      req(authOutput()$uid)
      academyUploadServer("academyUpload")
      
    } else if(!loadedPage$create & input$tabs == "createplayer"){
      req(authOutput()$uid)
      createPlayerServer("createplayer", userinfo = authOutput(), parent = session)
      loadedPage$create <- TRUE
      
    } else if(!loadedPage$player & input$tabs == "yourPlayer"){
      req(authOutput()$uid)
      yourPlayerServer("yourPlayer", uid = authOutput()$uid, parent = session, updated = updated)
      loadedPage$player <- TRUE
      
    # } else if(!loadedPage$contractProcess & input$tabs == "contractProcess"){
    #   req(authOutput()$uid)
    #   contractProcessServer("contractProcess", uid = authOutput()$uid)
    #   loadedPage$contractProcess <- TRUE
    #   
    # } else if(!loadedPage$tradeProcess & input$tabs == "tradeProcess"){
    #   req(authOutput()$uid)
    #   tradeProcessServer("tradeProcess", uid = authOutput()$uid)
    #   loadedPage$tradeProcess <- TRUE
      
    } else if(!loadedPage$playerPages & input$tabs == "playerPages"){
      playerPagesServer("playerPages")
      loadedPage$playerPages <- TRUE
      
    } else if(!loadedPage$playerEdit & input$tabs == "playerEdit"){
      req(authOutput())
      playerEditServer("playerEdit", uid = authOutput()$uid)
      loadedPage$playerEdit <- TRUE
      
    } else if(!loadedPage$leagueStandings & input$tabs == "leagueStandings"){
      leagueStandingsServer("leagueStandings")
      loadedPage$leagueStandings <- TRUE
      
    } else if(!loadedPage$leagueSchedule & input$tabs == "leagueSchedule"){
      leagueScheduleServer("leagueSchedule")
      loadedPage$leagueSchedule <- TRUE
      
    } else if(!loadedPage$academyIndex & input$tabs == "academyIndex"){
      academyIndexServer("academyIndex")
      loadedPage$academyIndex <- TRUE
      
    } else if(!loadedPage$index & input$tabs == "leagueindex"){
      leagueIndexServer("leagueindex")
      loadedPage$index <- TRUE
      
    } else if(!loadedPage$records & input$tabs == "careerRecords"){
      careerRecordsServer("careerRecords")
      loadedPage$careerRecords <- TRUE
      
    } else if(!loadedPage$uploadGame & input$tabs == "uploadGame"){
      uploadGameServer("uploadGame")
      loadedPage$uploadGame <- TRUE
    
    } else if(!loadedPage$managerTeam & input$tabs == "managerTeam"){
      req(authOutput())
      managerTeamServer("managerTeam", userinfo = authOutput())
      loadedPage$managerTeam <- TRUE
      
    } else if(!loadedPage$assignManager & input$tabs == "assignManager"){
      req(authOutput())
      assignManagerServer("assignManager", userinfo = authOutput())
      loadedPage$assignManager <- TRUE
      
    } else if(!loadedPage$bodoverview & input$tabs == "bodoverview"){
      req(authOutput())
      bodTeamServer("bodoverview", userinfo = authOutput())
      loadedPage$bodoverview <- TRUE
      
    } else if(!loadedPage$exportBuild & input$tabs == "exportBuild"){
      exportBuildServer("exportBuild")
      loadedPage$exportBuild <- TRUE
    
    } else if(input$tabs == "organizationPages"){
      organizationPagesServer("organizationPages")
      
    } else if(!loadedPage$draftClass & input$tabs == "draftClass"){
      draftClassServer("draftClass")
      loadedPage$draftClass <- TRUE
    
    } else if(!loadedPage$nationTracker & input$tabs == "nationTracker"){
      nationTrackerServer("nationTracker")
      loadedPage$nationTracker <- TRUE
    
    } else if(!loadedPage$positionTracker & input$tabs == "positionTracker"){
      positionTrackerServer("positionTracker")
      loadedPage$positionTracker <- TRUE
      
    } else if(!loadedPage$bankOverview & input$tabs == "bankOverview"){
      req(authOutput())
      bankOverviewServer("bankOverview", uid = authOutput()$uid, parent = session, updated = updated)
      loadedPage$bankOverview <- TRUE
      
    } else if(!loadedPage$academyStandings & input$tabs == "academyStandings"){
      academyStandingsSERVER("academyStandings")
      loadedPage$academyStandings <- TRUE
      
    # } else if(input$tabs == "budgetOverview"){
    #   budgetOverviewServer("budgetOverview")
      
    }
    
  }) %>% 
    bindEvent(input$tabs)
  
}
# Run the application 
app <- shinyApp(ui = ui, server = server, enableBookmarking = "url")
