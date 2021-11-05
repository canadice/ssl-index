
############################################################################
############################################################################
###                                                                      ###
###                 POSITION TRACKER CREATED FOR THE SHL                 ###
###                                                                      ###
############################################################################
############################################################################


### UI module for player similarities using MDS
playerAttributesUI <- function(id){
  
  ## Creating the namespacing function for all IDs
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 10,
          offset = 1,
          tabBox(
            width = NULL,
            selected = "Player Stats",
            tabPanel(
              "Player Stats",
              DT::DTOutput(
                outputId = ns("playerStats")
              )
            ),
            tabPanel(
              "Leaders",
              fluidRow(
                column(
                  width = 5,
                  offset = 1,
                  DT::DTOutput(
                    outputId = ns("leaderGoals")
                  )
                ),
                column(
                  width = 5,
                  DT::DTOutput(
                    outputId = ns("leaderAssists")
                  )
                )
              ),
              br(),
              fluidRow(
                column(
                  width = 5,
                  offset = 1,
                  DT::DTOutput(
                    outputId = ns("leaderSoT")
                  )
                ),
                column(
                  width = 5,
                  DT::DTOutput(
                    outputId = ns("leaderInterceptions")
                  )
                )
              ),
              br(),
              fluidRow(
                column(
                  width = 5,
                  offset = 1,
                  DT::DTOutput(
                    outputId = ns("leaderFouls")
                  )
                ),
                column(
                  width = 5,
                  DT::DTOutput(
                    outputId = ns("leaderPoM")
                  )
                )
              )
            )
          )
        )
      )
    )
 )
}

## Backend module for player similarities
playerAttributesSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){

      ###  Reading the data
           
      url <- "https://raw.githack.com/canadice/ssl-index/main/SSL-Index/data/playerStats.html"
      
      ###  Processing the data                                         
      
      playerAttributes <- 
        url %>% 
        read_html() %>% 
        html_elements("table") %>% 
        html_table() %>% 
        .[[1]] %>% 
        
        
      
      output$playerAttributes <- renderDT({
        if(length(playerAttributes) == 0){
          NULL
        } else {
          datatable(
            playerAttributes,
            style = "bootstrap",
            class = 'compact cell-border stripe',
            rownames = FALSE,
            escape = FALSE,
            options = 
              list(
                ordering = TRUE, 
                ## Sets a scroller for the rows
                scrollY = '80%',
                sScrollX = "100%",
                ## Sets size of rows shown
                scrollCollapse = TRUE,
                pageLength = 10,
                dom = 'Rftp',
                ## Sets color of table background
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#00044d', 'color': '#fff'});",
                  "}")
              )
          )  
        }
      })
      
    }
  )
}

