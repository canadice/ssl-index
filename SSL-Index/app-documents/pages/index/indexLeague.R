leagueIndexUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      ## First row
      fluidRow(
        column(
          width = 2,
          selectInput(
            inputId = ns("selectedSeason"),
            label = "Select a season",
            choices = 
              1:currentSeason$season %>% 
              sort(decreasing = TRUE)
          )
        ),
        column(
          width = 8
        ),
        column(
          width = 2,
          uiOutput(ns("leagueSelector"))
        )
      ),
      ## Second row
      fluidRow(
        tabsetPanel(
          tabPanel("TEST", 
                   reactableOutput(ns("outfieldBasic")) %>% 
                     withSpinnerMedium()),
          tabPanel("Out", h4("TASTA"))
        )
      ),
      fluidRow(
        verbatimTextOutput(ns("user"))
      )
    )
  )
}

leagueIndexServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #### DATA GENERATION ####
      outfieldData <- reactive({
        getOutfieldIndex(league = input$selectedLeague, season = input$selectedSeason) 
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
      
      #### REACTABLE OUTPUT ####
      output$outfieldBasic <- renderReactable({
        outfieldData() %>% 
          then(
            onFulfilled = function(data){
              data %>% 
              reactable(
                pagination = TRUE,
                columns =
                  list(
                    name = colDef(
                      minWidth = 250,
                      class = "stickyReactableColumn",
                      headerClass = "stickyReactableHeader",
                      cell = 
                        function(value, index){
                          Club <- data %>% 
                            .$club %>% 
                            .[index]
                          
                          if(Club %>% str_detect(",")){
                            clubs <- str_split(Club, pattern = ",", simplify = TRUE) %>% c() %>% rev()
                            
                            list <- 
                              tagList(
                                lapply(
                                  clubs,
                                  function(X){
                                    div(
                                      style = "display: inline-block; width: 25px;", 
                                      img(src = sprintf("%s.png", X), style = "height: 25px;", alt = X) 
                                    )
                                  }
                                )
                              )
                            
                          } else {
                            # file.exists(sprintf("%s.png", Club)) %>% print()
                            
                            image <- img(src = sprintf("%s.png", Club), style = "height: 25px;", alt = Club)  
                            
                            list <- 
                              tagList(
                                div(style = "display: inline-block; width: 25px;", image)
                              )
                          }
                          
                          tagList(
                            div(style = "display: inline-block; width: 250px;", value),
                            list
                          )
                        }
                    ),
                    club = 
                      colDef(
                        show = FALSE,
                        searchable = TRUE
                      )
                  ) %>% 
                  append(
                    pmap(statisticsLegend, ~ {
                      if((..1) %in% names(data)) {
                        ..1 = 
                          colDef(
                            header = 
                              withTooltip(
                                ..1, 
                                ..2),
                            html = TRUE
                          )
                      }
                    }) %>% 
                      setNames(statisticsLegend$statistic) %>% 
                      Filter(Negate(is.null), .)
                  )
              )
            }
          )
          
      })  
      
      output$user <- renderPrint({ 
      })
      
    }
  )
}