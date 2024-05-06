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
            label = "Season",
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
          selectInput(
            inputId = ns("selectedLeague"),
            label = "League",
            choices = 
              c(
                "1",
                "2",
                "Cup",
                "WSFC"
              )
          )
        )
      ),
      ## Second row
      fluidRow(
        reactableOutput(ns("outfield"))  
      ),
      fluidRow(
        verbatimTextOutput(ns("user"))
      )
    )
  )
}

leagueIndexServer <- function(id, userinfo) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$outfield <- renderReactable({
        data <- getOutfieldIndex(league = input$selectedLeague, season = input$selectedSeason) 
        
        data %>% 
          reactable(
            columns =
              list(
                name = colDef(
                  minWidth = 250,
                  style = list(position = "sticky", left = 0, background = "#F8F8F8", zIndex = 1),
                  headerStyle = list(position = "sticky", left = 0, zIndex = 1, background = "white"),
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
      })  
      
      output$user <- renderPrint({ 
      })
      
    }
  )
}