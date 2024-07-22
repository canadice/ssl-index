advancedStatsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      h1("Advanced Statistics", align = "center"),
      fluidRow(
        column(
          width = 6,
          inputPanel(
            selectInput(
              inputId = ns("category"),
              label = "Select type of data",
              choices = c("Player" = "Name", "Club")
              ),
            radioButtons(
              inputId = ns("adjustment"),
              label = "Adjust for penalty kicks
              (not working properly)",
              choices = c("Yes" = TRUE, "No" = FALSE)
              )
          )
        )
      ),
      fluidRow(
        tabBox(
          width = 12,
          tabPanel(
            "Luck",
            fluidRow(
              column(
                width = 12,
                p("Using the number of expected goals (xG) and the number of actual goals 
                  (aG) scored, it is possible to calculate something close to the 'luck' 
                  of a player or a team. xG is a metric that shows how likely a shot from 
                  a specific spot on the pitch is to go into the net, where every shot is 
                  given a value between 0 (highly unlikely) to 1 (very likely). If a player
                  or team has scored more goals than what they are expected to do they are 
                  considered lucky (high Offensive Luck), whereas if a player or team has 
                  conceded fewer goals than what they are expected to do they are considered
                  lucky (high Defensive Luck)."),
                p("In order to compare data the values are calculated on a per game basis,
                due to the number of games are different across seasons and teams.")
              )
            ),
            fluidRow(
              uiOutput(
                outputId = ns("unluckiest")
              ),
              uiOutput(
                outputId = ns("luckiest")
              )
            ),
            reactableOutput(
              outputId = ns("luck")
            ) %>% withSpinner() 
          )
        ) 
      )
    )
  )
}

advancedStatsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      luckData <- 
        reactive({
          res <-  
            GET(
              url = "https://api.simulationsoccer.com/ssl/advancedStatsLuck",
              query = list(category = input$category, penaltyAdjust = input$adjustment)
              )
          
          fromJSON(res$content %>% rawToChar())
        })
      
      output$luck <- renderReactable({
        luckData() %>% 
          reactable(
            searchable = TRUE,
            theme = pff(),
            #### Column stylings ####
            defaultColDef = 
              colDef(
                format = colFormat(digits = 3),
                style = "
                white-space: nowrap;
                text-overflow: ellipsis;
                "),
            columns = 
              list(
                Name = 
                  colDef(
                    width = 200
                  ),
                Club = 
                  colDef(
                    minWidth = 45,
                    cell = function(value, index){
                      Season <- luckData()$Season[index]
                      
                      if(value %>% str_detect("&")){
                        clubs <- str_split(value, pattern = " & ", simplify = TRUE) %>% c()
                        
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
                        image <- img(src = sprintf("%s.png", value), style = "height: 25px;", alt = value)  
                        
                        list <- 
                          tagList(
                            div(style = "display: inline-block; width: 25px;", image)
                          )
                      }
                      
                      list %>% 
                        tagList(
                          div(style = "font-size: 1rem", paste("Season", Season))
                        )
                    }
                  ),
                Season = colDef(show = FALSE)#,
                # `Luck Offensive` =
                #   colDef(
                #     cell = 
                #       data_bars(
                #         data = luckData(),
                #         text_position = 'outside-end',
                #         fill_color = brewer.pal(5, name = "Spectral")
                #       )
                #   ),
                # `Luck Defensive` =
                #   colDef(
                #     cell = 
                #       data_bars(
                #         data = luckData(),
                #         text_position = 'outside-end',
                #         fill_color = brewer.pal(5, name = "Spectral")
                #       )
                #   )
              )
            #### END ####
          )
      })
      
      
      
      
      
    }
  )
}