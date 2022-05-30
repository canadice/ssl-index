
############################################################################
############################################################################
###                                                                      ###
###                  LEAGUE STANDINGS FOR THE SSL INDEX                  ###
###                                                                      ###
############################################################################
############################################################################


### UI module for player similarities using MDS
standingsUI <- function(id){
  
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
              1:length(url) %>% 
              sort(decreasing = TRUE)
          )
        )
      ),
      fluidRow(
        column(
          width = 10,
          offset = 1,
          DTOutput(outputId = ns("standings"))
        )
      )
    )
 )
}

## Backend module for player similarities
standingsSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){
      
      output$standings <- renderDT({
        datatable(
          standings[[input$season %>% as.numeric()]], 
          style = "bootstrap",
          class = 'compact cell-border stripe',
          rownames = FALSE,
          escape = FALSE,
          options = 
            list(
              ordering = TRUE, 
              ## Sets size of rows shown
              scrollCollapse = TRUE,
              pageLength = 20,
              dom = 'Rt',
              columnDefs = 
                list(
                  list(
                    targets = 10:11,
                    visible = FALSE
                  )
                )
            )
        ) %>% 
          formatStyle(
            columns = 1:10,
            valueColumns = "color.primary",
            backgroundColor = 
              styleEqual(
                sort(unique(teamInfo$color.primary)), 
                sort(unique(teamInfo$color.primary))
              )
          ) %>% 
          formatStyle(
            columns = 1:10,
            valueColumns = "color.secondary",
            color = 
              styleEqual(
                sort(unique(teamInfo$color.secondary)), 
                sort(unique(teamInfo$color.secondary))
              )
          )
      })
    }
  )
}

