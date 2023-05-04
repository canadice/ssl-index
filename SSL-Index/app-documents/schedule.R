
###########################################################################
###########################################################################
###                                                                     ###
###                  LEAGUE SCHEDULE FOR THE SSL INDEX                  ###
###                                                                     ###
###########################################################################
###########################################################################


### UI module for player similarities using MDS
scheduleUI <- function(id){
  
  ## Creating the namespacing function for all IDs
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 2,
          selectInput(
            inputId = ns("season"),
            label = "Select season",
            choices = 
              1:(playerData %>% 
                select(Class) %>% 
                mutate(
                  Class = str_extract(Class, pattern = "[0-9]+") %>% as.numeric()
                ) %>% 
                filter(
                  Class == max(Class)
                ) %>% 
                unique() %>% 
                unlist() - 1) %>% 
              sort(decreasing = TRUE)
          ),
          uiOutput(
            outputId = ns("divisionFilter")
          )
        ),
        column(
          width = 10,
          reactableOutput(
            outputId = ns("schedule")
          ) %>% 
            withSpinner()
        )
      )
    )
 )
}

## Backend module for player similarities
scheduleSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){
      
      output$divisionFilter <- renderUI({
        if(input$season < 5){
          NULL
        } else {
          selectInput(
            inputId = session$ns("divisionFilter"),
            label = "Select division",
            choices = c("All", "1", "2"),
            selected = "All"
          )
        }
      })
     
      schedule <- reactive({
        schedule <- 
          read_sheet(
            ss = "https://docs.google.com/spreadsheets/d/1jcsFLjtiq-jK273DI-m-N38x9yUS66HwuX5x5Uig8Uc/edit?usp=sharing", 
            sheet = paste("Season", input$season)
          ) %>% 
          mutate(
            `In-game Date` = `In-game Date` %>% as_date() %>% format(format = "%m/%d"),
            `IRL Date` = `IRL Date` %>% as_date() %>% format(format = "%m/%d")
          )
        
        if(input$season < 5){
          schedule
        } else {
          if(input$divisionFilter != "All"){
            schedule %>% 
              filter(
                Division %in% c(input$divisionFilter, "Pre", "Cup", "Shield")
              )  
          } else {
            schedule
          }
        }
        
      })
      
      output$schedule <- renderReactable({
        if(length(schedule()) == 0){
          stop("The current schedule file is empty. Please notify the owners.")
          # NULL
        } else {
          schedule() %>% 
            reactable(
              theme = pff(font_color = "#000"),
              pagination = FALSE,
              columns = 
                list(
                  `IRL Date` = colDef(width = 75),
                  Division = colDef(width = 75),
                  Matchday = colDef(width = 150),
                  Home = 
                    colDef(
                      cell = function(value){
                        image <- img(src = sprintf("%s.png", value), style = "height: 25px;", alt = value)
                        
                        tagList(
                          div(style = "display: inline-block; width: 25px;", image),
                          div(style = "font-size: 1rem", value)
                        )
                      }
                    ),
                  Away = 
                    colDef(
                      cell = function(value){
                        image <- img(src = sprintf("%s.png", value), style = "height: 25px;", alt = value)
                        
                        tagList(
                          div(style = "display: inline-block; width: 25px;", image),
                          div(style = "font-size: 1rem", value)
                        )
                      }
                    )
                )
            )
        }
      })
       
    }
  )
}

