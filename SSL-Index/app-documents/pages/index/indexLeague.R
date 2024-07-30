leagueIndexUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      ## First row
      fluidRow(
        column(
          width = 4,
          selectInput(
            inputId = ns("selectedSeason"),
            label = "Select a season",
            choices = 
              1:currentSeason$season %>% 
              sort(decreasing = TRUE)
          )
        ),
        column(
          width = 6
        ),
        column(
          width = 2,
          uiOutput(ns("leagueSelector"))
        )
      ),
      ## Second row
      fluidRow(
        h1("Outfield"),
        tabsetPanel(
          tabPanel("Statistics",
                   reactableOutput(ns("outfieldBasic")) %>% 
                     withSpinnerMedium()),
          tabPanel("Adv. Statistics",
                   reactableOutput(ns("outfieldAdvanced")) %>% 
                     withSpinnerMedium()),
          tabPanel("Leaders",
                   uiOutput(ns("outfieldLeaders")) %>% 
                     withSpinnerMedium())
          )
        ),
      ## Third row
      fluidRow(
        h1("Keeper"),
        tabsetPanel(
          tabPanel("Statistics",
                   reactableOutput(ns("keeperBasic")) %>% 
                     withSpinnerMedium()),
          tabPanel("Adv. Statistics",
                   reactableOutput(ns("keeperAdvanced")) %>% 
                     withSpinnerMedium()),
          tabPanel("Leaders",
                   uiOutput(ns("keeperLeaders")) %>% 
                     withSpinnerMedium())
        )
      )
    ) # close fluidpage
  ) # close tagList
}

leagueIndexServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #### DATA GENERATION ####
      outfieldData <- reactive({
        req(input$selectedLeague)
        
        readAPI(url = "https://api.simulationsoccer.com/index/outfield", 
                query = list(league = input$selectedLeague, season = input$selectedSeason)
        ) %>% 
          future_promise()
      })
      
      keeperData <- reactive({
        req(input$selectedLeague)
        readAPI(url = "https://api.simulationsoccer.com/index/keeper", 
                query = list(league = input$selectedLeague, season = input$selectedSeason)
        ) %>% 
          future_promise()
      })
      
      #### UI OUTPUT ####
      output$leagueSelector <- renderUI({
        season <- input$selectedSeason %>% as.numeric()
        
        if(season < 5){
          selectInput(
            inputId = session$ns("selectedLeague"),
            label = "League",
            choices = 
              c(
                "ALL",
                "League" = "1",
                "Cup"
              )
          )
        } else if (season == 12){
          selectInput(
            inputId = session$ns("selectedLeague"),
            label = "League",
            choices = 
              c(
                "ALL",
                "Major" = "1",
                "Minor" = "2",
                "Cup",
                "WSFC"
              )
          )
        } else if (season < 12){
          selectInput(
            inputId = session$ns("selectedLeague"),
            label = "League",
            choices = 
              c(
                "ALL",
                "Division 1" = "1",
                "Division 2" = "2",
                "Cup"
              )
          )
        } else {
          selectInput(
            inputId = session$ns("selectedLeague"),
            label = "League",
            choices = 
              c(
                "ALL",
                "Major" = "1",
                "Minor" = "2",
                "Cup"
              )
          )
        }
        
        
      })
      
      outstatistics <- c("goals", "assists", "player of the match", "distance run (km)", "successful passes", "chances created", "tackles won", "interceptions", "yellow cards", "red cards")
      
      output$outfieldLeaders <- renderUI({
        # Split statistics into pairs
        statisticPairs <- split(outstatistics, (seq_along(outstatistics) - 1) %/% 2)
        
        # Create fluidRows for each pair
        lapply(statisticPairs, function(pair) {
          fluidRow(
            lapply(pair, function(stat) {
              column(width = 6,
                     reactableOutput(session$ns(paste0(stat, "_leader"))) %>% 
                       div(class = "leaderTable")
              )
            })
          )
        })
      })
      
      lapply(outstatistics, function(stat){
        output[[paste0(stat, "_leader")]] <- renderReactable({
          outfieldData() %>% 
            then(
              onFulfilled = function(data){
                data %>% 
                  select(
                    name, club, !!sym(stat)
                  ) %>% 
                  arrange(!!sym(stat) %>% desc()) %>% 
                  slice_head(n = 10) %>% 
                  leaderReactable()
              }
            )
        })
      })
      
      keepstatistics <- c("won", "clean sheets", "conceded", "save%")
      
      output$keeperLeaders <- renderUI({
        # Split statistics into pairs
        statisticPairs <- split(keepstatistics, (seq_along(keepstatistics) - 1) %/% 2)
        
        # Create fluidRows for each pair
        lapply(statisticPairs, function(pair) {
          fluidRow(
            lapply(pair, function(stat) {
              column(width = 6,
                     reactableOutput(session$ns(paste0(stat, "_leader"))) %>% 
                       div(class = "leaderTable")
              )
            })
          )
        })
      })
      
      lapply(keepstatistics, function(stat){
        output[[paste0(stat, "_leader")]] <- renderReactable({
          keeperData() %>% 
            then(
              onFulfilled = function(data){
                data %>% 
                  select(
                    name, club, !!sym(stat)
                  ) %>% 
                  arrange(!!sym(stat) %>% desc()) %>% 
                  slice_head(n = 10) %>% 
                  leaderReactable()
              }
            )
        })
      })
      
      #### REACTABLE OUTPUT ####
      output$outfieldBasic <- renderReactable({
        outfieldData() %>% 
          then(
            onFulfilled = function(data){
              currentData <- 
                data %>% 
                select(
                  name:assists, `shots on target`:offsides, blocks, `shots blocked`, `average rating`
                ) 
              
              currentData %>% 
                indexReactable()
            }
          )
          
      })  
      
      output$outfieldAdvanced <- renderReactable({
        outfieldData() %>% 
          then(
            onFulfilled = function(data){
              currentData <- 
                data %>% 
                select(
                  name:club, 
                  xg,
                  xa:`fk shots`,
                  `open play key passes`:`goals outside box`,
                  `press%`:`pen adj xG`
                ) 
              
              currentData %>% 
                indexReactable()
            }
          )
        
      }) 
      
      output$keeperBasic <- renderReactable({
        keeperData() %>% 
          then(
            onFulfilled = function(data){
              currentData <- 
                data %>% 
                select(
                  name:`save%`
                ) 
              
              currentData %>% 
                indexReactable()
            }
          )
        
      })  
      
      output$keeperAdvanced <- renderReactable({
        keeperData() %>% 
          then(
            onFulfilled = function(data){
              currentData <- 
                data %>% 
                select(
                  name:club, 
                  `penalties faced`:`xg prevented`
                ) 
              
              currentData %>% 
                indexReactable()
            }
          )
        
      }) 
      
    }
  )
}