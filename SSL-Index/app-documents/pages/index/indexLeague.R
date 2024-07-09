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
                     withSpinnerMedium()))
        ),
      fluidRow(
        h1("Keeper"),
        tabsetPanel(
          tabPanel("Statistics",
                   reactableOutput(ns("keeperBasic")) %>% 
                     withSpinnerMedium()),
          tabPanel("Adv. Statistics",
                   reactableOutput(ns("keeperAdvanced")) %>% 
                     withSpinnerMedium()))
        )
    ) # close fluidpage
  ) # close tagList
}

leagueIndexServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #### REUSABLE REACTABLE FUNCTION ####
      indexReactable <- function(currentData){
        statisticsTooltips <- statisticsLegend[statisticsLegend$statistic %in% colnames(currentData),]
        
        currentData %>%
          mutate(
            across(
              where(is.numeric),
              ~ round(.x, 2)
            )
          ) %>% 
          reactable(
            pagination = TRUE,
            searchable = TRUE,
            defaultColDef = colDef(minWidth = 100, maxWidth = 250),
            columns =
              list(
                name = colDef(
                  name = "PLAYER",
                  minWidth = 250,
                  class = "stickyReactableColumn",
                  headerClass = "stickyReactableHeader",
                  cell = 
                    function(value, index){
                      Club <- currentData %>% 
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
                        div(
                          class = "tableClubName",
                          list,
                          span(value)
                        )
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
                pmap(statisticsTooltips, ~ {
                  if((..1) %in% names(currentData)) {
                    ..1 =
                      colDef(
                        header =
                          tippy(..1 %>% str_to_upper(), ..2, placement = "top", theme = "material"),
                        html = TRUE
                      )
                  }
                }) %>%
                  setNames(statisticsTooltips$statistic) %>%
                  Filter(Negate(is.null), .)
              ) 
          )
      }
      
      #### DATA GENERATION ####
      outfieldData <- reactive({
        req(input$selectedLeague)
        getOutfieldIndex(league = input$selectedLeague, season = input$selectedSeason) 
      })
      
      keeperData <- reactive({
        req(input$selectedLeague)
        getKeeperIndex(league = input$selectedLeague, season = input$selectedSeason) 
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