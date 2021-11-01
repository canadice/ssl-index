
############################################################################
############################################################################
###                                                                      ###
###                 POSITION TRACKER CREATED FOR THE SHL                 ###
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
          offset = 1,
          DTOutput(outputId = ns("standings")),
          DTOutput(outputId = ns("standings2"))
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
     
      url <- "https://raw.githack.com/canadice/ssl-index/main/SSL-Index/data/standingsfm.html"
      
      standings <- 
        url %>% 
        read_html() %>% 
        html_elements("table") %>% 
        html_table()
     
      output$standings <- renderDT({
        standings[[1]]
      })
      
      output$standings2 <- renderDT({
        standings[[2]]
      })
       
    }
  )
}

