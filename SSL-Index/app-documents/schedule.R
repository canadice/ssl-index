
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
              1:4 %>% 
              sort(decreasing = TRUE)
          )
        ),
        column(
          width = 10,
          DTOutput(outputId = ns("schedule"))
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
     
      schedule <- reactive({
        read_sheet(
          ss = "https://docs.google.com/spreadsheets/d/1jcsFLjtiq-jK273DI-m-N38x9yUS66HwuX5x5Uig8Uc/edit?usp=sharing", 
          sheet = paste("Season", input$season)
        ) %>% 
          mutate(
            `In-game Date` = `In-game Date` %>% as_date() %>% format(format = "%m/%d"),
            `IRL Date` = `IRL Date` %>% as_date() %>% format(format = "%m/%d")
          )
      })
      
      output$schedule <- renderDT({
        if(length(schedule()) == 0){
          stop("The current schedule file is empty. Please notify the owners.")
          # NULL
        } else {
          datatable(
            schedule(),
            style = "bootstrap",
            class = 'compact cell-border stripe',
            rownames = FALSE,
            escape = FALSE,
            options = 
              list(
                ordering = FALSE, 
                ## Sets a scroller for the rows
                scrollY = '80%',
                sScrollX = "100%",
                ## Sets size of rows shown
                scrollCollapse = TRUE,
                pageLength = 999,
                dom = 'Rt',
                ## Sets color of table background
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#00044d', 'color': '#fff'});",
                  "}")
              )
          ) %>% 
            formatStyle(
              columns = c("Home", "Away"),
              # backgroundColor = 
              #   styleEqual(
              #     levels = teamInfo$team,
              #     values = teamInfo$color.primary  
              #   ),
              color = 
                styleEqual(
                  levels = teamInfo$team,
                  values = teamInfo$color_primary 
                )
            ) 
            
        }
      })
       
    }
  )
}

