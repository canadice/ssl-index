
###########################################################################
###########################################################################
###                                                                     ###
###                              SSL INDEX                              ###
###                                                                     ###
###########################################################################
###########################################################################

## Data handling from HTML format
require(XML)

## Visualizations
require(ggplot2)
require(ggnewscale)
require(RColorBrewer)
require(cowplot)

## Package for handling date and time
require(lubridate)

## Packages for handling strings
require(stringr)
require(stringi)

## Loading package that can talk to Google Sheets
require(googlesheets4)
require(googledrive)

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

ui <- 
  dashboardPage(
    title = "SSL Index",
    dashboardHeader(
      # title = customLogo,
      tags$li(
        class = "dropdown",
        tags$head(
          ## HTML code so that a href link inherits the text color, not the link color
          tags$style(HTML("a, a:hover, a:visited, a:active {color: inherit}")),
          
          ## Increases the size of the logo box at the top left
          tags$style(".main-header {max-height: 80px}"),
          tags$style(".main-header .logo {height: 80px}"),
          tags$style(".main-header .logo {width: 300px}"),
          
          ## Changes the margin of the sidebar
          tags$style(".main-header .navbar {margin-left: 300px}"),
          tags$style(type="text/css", "text {font-family: sans-serif, courier}"),
          
        )
      )
    ),
    dashboardSidebar(
      width = 300,
      # Adjust the sidebar in accordance with the higher header
      tags$style(".left-side, .main-sidebar {padding-top: 100px}"),
      sidebarMenu(
        id = "tabs",
        
        #################################################################
        ##                           Welcome                           ##
        #################################################################
        
        menuItem(
          "Welcome",
          tabName = "welcome",
          selected = TRUE
        ),
        
        menuItem(
          "Schedule",
          tabName = "schedule"
        ),
        menuItem(
          "Standings",
          tabName = "standings"
        ),
        menuItem(
          "Player Statistics",
          tabName = "playerStats"
        ),
        menuItem(
          "Github", 
          icon = icon("github"),
          href = "https://github.com/canadice/ssl-index"
        )
      )
    ),
    dashboardBody(
      # customTheme,
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
          "playerStats",
          titlePanel(
            h1("Player Stats", align = "center")
          ),
          playerStatsUI(id = "playerStats")
        )
      )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  loadedModuleSchedule <- reactiveVal(FALSE)
  loadedModuleStandings <- reactiveVal(FALSE)
  loadedModulePlayerStats <- reactiveVal(FALSE)
  # loadedModuleIIHF <- reactiveVal(FALSE)
  # loadedModuleIIHF <- reactiveVal(FALSE)
  # loadedModuleIIHF <- reactiveVal(FALSE)
  # loadedModuleIIHF <- reactiveVal(FALSE)
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
      
    # } else if(input$tabs == "trackerTeam" & !loadedModuleTeam()){
    #   
    #   loadedModuleTeam(TRUE)
    #   
    #   teamSERVER(id = "teamUI")
    #   
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)