
###########################################################################
###########################################################################
###                                                                     ###
###                              SSL INDEX                              ###
###                                                                     ###
###########################################################################
###########################################################################

## Data handling from HTML format
# remotes::install_github("Canadice/sslrtools")
require(sslrtools)

require(rvest)

## Data handling
require(dplyr)
require(tidyr)

## Visualizations
require(ggplot2)
require(ggnewscale)
require(RColorBrewer)
require(cowplot)
require(plotly)
require(ggimage)
require(magick)
require(rsvg)
require(grid)
require(ggpubr)
require(ggforce)

## Tables
require(formattable)
# require(gt)
# require(gtExtras) #Github package
require(reactable)


## Package for handling date and time
require(lubridate)

## Packages for handling strings
require(stringr)
require(stringi)

## Loading package that can talk to Google Sheets
require(googlesheets4)
require(googledrive)

## Loading packages for handling RMarkdown files
require(rmarkdown)
require(markdown)

##Loading Database packages for SQLite database
require(DBI)
require(dbplyr)
require(RSQLite)

## Loading Shiny packages
require(shiny)
require(DT)
require(knitr)
require(kableExtra)
# require(shinythemes)
require(shinycssloaders)
require(shinyjs)
require(shinydashboard)
require(dashboardthemes)
require(shiny.router)

##################################################################
##                      SSL Logo and Theme                      ##
##################################################################


sslBlueD <- "#070B51"
sslBlueL <- "#141204"
sslGold <- "#BD9523"

# customLogo <- 
#   shinyDashboardLogoDIY(
#     boldText = "",
#     mainText = tags$a(
#       href='https://simulationhockey.com/',
#       target="_blank",
#       tags$img(src='shl_analytics_logo.png', height = "70"),
#     ),
#     badgeText = version,
#     badgeTextColor = "white",
#     badgeBackColor = sslRed
#   )

customTheme <- 
  shinyDashboardThemeDIY(
    
    ### general
    appFontFamily = "Arial"
    ,appFontColor = "#2D2D2D"
    ,primaryFontColor = "#FFFFFF"
    ,infoFontColor = "#0F0F0F"
    ,successFontColor = "#FFFFFF"
    ,warningFontColor = "#0F0F0F"
    ,dangerFontColor = "#0F0F0F"
    ,bodyBackColor = "#F5F5F5"
    
    ### header
    ,logoBackColor = sslBlueD
    
    ,headerButtonBackColor = sslBlueD
    ,headerButtonIconColor = sslGold
    ,headerButtonBackColorHover = "#FFFFFF"
    ,headerButtonIconColorHover = "#000000"
    
    ,headerBackColor = sslBlueD
    ,headerBoxShadowColor = ""
    ,headerBoxShadowSize = "0px 0px 0px"
    
    ### sidebar
    ,sidebarBackColor = sslBlueD
    ,sidebarPadding = "0"
    
    ,sidebarMenuBackColor = "transparent"
    ,sidebarMenuPadding = "10"
    ,sidebarMenuBorderRadius = 0
    
    ,sidebarShadowRadius = ""
    ,sidebarShadowColor = "0px 0px 0px"
    
    ,sidebarUserTextColor = sslGold
    
    ,sidebarSearchBackColor = sslGold
    ,sidebarSearchIconColor = "#FFFFFF"
    ,sidebarSearchBorderColor = "#FFFFFF"
    
    ,sidebarTabTextColor = "#FFFFFF"
    ,sidebarTabTextSize = "14"
    ,sidebarTabBorderStyle = "none"
    ,sidebarTabBorderColor = "none"
    ,sidebarTabBorderWidth = "0"
    
    ,sidebarTabBackColorSelected = sslGold
    ,sidebarTabTextColorSelected = "#FFFFFF"
    ,sidebarTabRadiusSelected = "0px"
    
    ,sidebarTabBackColorHover = "#FFFFFF"
    ,sidebarTabTextColorHover = "#000000"
    ,sidebarTabBorderStyleHover = "none solid none none"
    ,sidebarTabBorderColorHover = "#FFFFFF"
    ,sidebarTabBorderWidthHover = "10"
    ,sidebarTabRadiusHover = "0px"
    
    ### boxes
    ,boxBackColor = "#F8F8F8"
    ,boxBorderRadius = "5"
    ,boxShadowSize = "none"
    ,boxShadowColor = ""
    ,boxTitleSize = "18"
    ,boxDefaultColor = "#E1E1E1"
    ,boxPrimaryColor = sslGold
    ,boxInfoColor = "#B4B4B4"
    ,boxSuccessColor = sslBlueL
    ,boxWarningColor = "#ED7D31"
    ,boxDangerColor = "#E84C22"
    
    ,tabBoxTabColor = "#F8F8F8"
    ,tabBoxTabTextSize = "14"
    ,tabBoxTabTextColor = "#646464"
    ,tabBoxTabTextColorSelected = "#2D2D2D"
    ,tabBoxBackColor = "#F8F8F8"
    ,tabBoxHighlightColor = sslBlueL
    ,tabBoxBorderRadius = "5"
    
    ### inputs
    ,buttonBackColor = "#D7D7D7"
    ,buttonTextColor = "#2D2D2D"
    ,buttonBorderColor = "#969696"
    ,buttonBorderRadius = "5"
    
    ,buttonBackColorHover = "#BEBEBE"
    ,buttonTextColorHover = "#000000"
    ,buttonBorderColorHover = "#969696"
    
    ,textboxBackColor = "#FFFFFF"
    ,textboxBorderColor = "#767676"
    ,textboxBorderRadius = "5"
    ,textboxBackColorSelect = "#F5F5F5"
    ,textboxBorderColorSelect = "#6C6C6C"
    
    ### tables
    ,tableBackColor = "#F8F8F8"
    ,tableBorderColor = "#EEEEEE"
    ,tableBorderTopSize = "1"
    ,tableBorderRowSize = "1"
  )


#################################################################
##               Running all modules for the app               ##
#################################################################

fileSources <- c("app-documents")

## Loads and runs RMarkdown files
rmdFiles <- 
  sapply(
    X = fileSources,
    FUN = function(x){
      list.files(path = x, pattern = ".Rmd$") %>% 
        paste(x, ., sep = "/")
    },
    simplify = TRUE,
    USE.NAMES = FALSE
  ) %>% 
  unlist() %>% 
  .[str_detect(., pattern = ".Rmd")]

sapply(rmdFiles, rmarkdown::render, quiet = T, output_dir = "app-documents")

## Loads files
sapply(
  X = fileSources,
  FUN = function(x){
    files <- list.files(path = x, pattern = ".R$")
    
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
    
    ##---------------------------------------------------------------
    ##                            Sidebar                           -
    ##---------------------------------------------------------------
    
    dashboardSidebar(
      # # Adjust the sidebar in accordance with the higher header
      # tags$style(".left-side, .main-sidebar {padding-top: 100px}"),
      sidebarMenu(
        id = "tabs",
        menuItem(
          "Welcome",
          tabName = "welcome",
          selected = TRUE
        ),
        menuItem(
          "SSL Index",
          menuSubItem(
            "Schedule",
            tabName = "schedule"
          ),
          menuSubItem(
            "Standings",
            tabName = "standings"
          ),
          menuSubItem(
            "Individual Statistics",
            tabName = "playerStats"
          )
        ),
        menuItem(
          "Teams",
          tabName = "teamOverview"
        ),
        menuItem(
          "Player Pages",
          tabName = "playerPages"
        ),
        menuItem(
          "Player Build Tools",
          menuSubItem(
            "Build a new player",
            tabName = "playerBuilder"
          ),
          menuSubItem(
            "Player Comparisons",
            tabName = "playerComparison"  
          ),
          menuSubItem(
            "Position Tracker",
            tabName = "trackerPosition"
          )
        ),
        menuItem(
          "SSL Forum",
          icon = icon("external-link-alt"),
          href = "https://simsoccer.jcink.net/index.php"
        ),
        menuItem(
          "Github", 
          icon = icon("github"),
          href = "https://github.com/canadice/ssl-index"
        )
      )
    ),
    
    ##----------------------------------------------------------------
    ##                              Body                             -
    ##----------------------------------------------------------------
    
    dashboardBody(
      customTheme,
      useShinyjs(),
      ### Specifies a custom color for value and info boxes
      tags$style(".small-box.bg-orange { background-color: #e08b46 !important; color: #000000 !important; }"),
      tabItems(
        tabItem(
          "welcome",
          welcomeUI(id = "welcome")
        ),
        tabItem(
          "schedule",
          titlePanel(
            h1("Schedule", align = "center")
          ),
          scheduleUI(id = "schedule")
        ),
        tabItem(
          "standings",
          titlePanel(
            h1("Standings", align = "center")
          ),
          standingsUI(id = "standings")
        ),
        tabItem(
          "teamOverview",
          titlePanel(
            h1("Team Overview", align = "center")
          ),
          teamOverviewUI(id = "teamOverview")
        ),
        tabItem(
          "playerStats",
          titlePanel(
            h1("Individual Stats", align = "center")
          ),
          playerStatsUI(id = "playerStats")
        ),
        tabItem(
          "playerComparison",
          titlePanel(
            h1("Player Comparison", align = "center")
          ),
          playerComparisonUI(id = "playerComparison")
        ),
        tabItem(
          "playerPages",
          titlePanel(
            h1("Player Pages", align = "center")
          ),
          playerDatabaseUI(id = "playerPages")
        ),
        tabItem(
          "trackerPosition",
          titlePanel(
            h1("Position Tracker", align = "center")
          ),
          trackerPositionUI(id = "trackerPosition")
        ),
        tabItem(
          "playerBuilder",
          titlePanel(
            h1("Player Builder", align = "center")
          ),
          hr(),
          p(
            paste("The Player Builder allows you to build your player",
              "using your earned TPE as a bank. The resulting build",
              "can then be exported to the Forum using the Export button.")
          ),
          hr(),
          playerBuilderUI(id = "playerBuilder")
        )
      )
    )
    ##----------------------------------------------------------------
  )
}

server <- function(input, output, session) {
  
  loadedModuleSchedule <- reactiveVal(FALSE)
  loadedModuleStandings <- reactiveVal(FALSE)
  loadedModulePlayerStats <- reactiveVal(FALSE)
  loadedModulePlayerComparison <- reactiveVal(FALSE)
  loadedModulePlayerBuilder <- reactiveVal(FALSE)
  loadedModuleTrackerPosition <- reactiveVal(FALSE)
  loadedModuleOverviewTeam <- reactiveVal(FALSE)
  loadedModulePlayerDatabase <- reactiveVal(FALSE)
  # loadedModuleIIHF <- reactiveVal(FALSE)
  
  
  ##---------------------------------------------------------------
  ##          Loading each of the different backend sites         -
  ##---------------------------------------------------------------
  ### Only run the module once the menu is clicked to fasten start time
  observeEvent(input$tabs,{
    ## Checks which menu tab has been selected and whether the module has already been loaded
    if(input$tabs == "schedule" & !loadedModuleSchedule()){
      
      loadedModuleSchedule(TRUE)
      
      scheduleSERVER(id = "schedule")
      
    } else if(input$tabs=="standings" & !loadedModuleStandings()){
      
      loadedModuleStandings(TRUE)
      
      standingsSERVER(id = "standings")
      
    } else if(input$tabs == "playerStats" & !loadedModulePlayerStats()){
      
      playerStatsSERVER(id = "playerStats")
      
    } else if(input$tabs == "playerComparison" & !loadedModulePlayerComparison()){

      loadedModulePlayerComparison(TRUE)

      playerComparisonSERVER(id = "playerComparison")

    } else if(input$tabs == "playerBuilder" & !loadedModulePlayerBuilder()){
      
      loadedModulePlayerBuilder(TRUE)
      
      playerBuilderSERVER(id = "playerBuilder")
      
    } else if(input$tabs == "trackerPosition" & !loadedModuleTrackerPosition()){
      
      loadedModuleTrackerPosition(TRUE)
      
      trackerPositionSERVER(id = "trackerPosition")
      
    } else if(input$tabs == "teamOverview" & !loadedModuleOverviewTeam()){
      
      loadedModuleOverviewTeam(TRUE)
      
      teamOverviewSERVER(id = "teamOverview")
      
    } else if(input$tabs == "playerPages" & !loadedModulePlayerDatabase()){
      
      loadedModulePlayerDatabase(TRUE)
      
      playerDatabaseSERVER(id = "playerPages")
      
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
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
  
  
}
# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "disable")
