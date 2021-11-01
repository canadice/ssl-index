
############################################################################
############################################################################
###                                                                      ###
###                 POSITION TRACKER CREATED FOR THE SHL                 ###
###                                                                      ###
############################################################################
############################################################################


### UI module for player similarities using MDS
scheduleUI <- function(id){
  
  ## Creating the namespacing function for all IDs
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 2,
          offset = 1,
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
     
      url <- "https://raw.githack.com/canadice/ssl-index/main/SSL-Index/data/scheduleFM.html"
      
      schedule <- 
        url %>% 
        read_html() %>% 
        html_elements("table") %>% 
        html_table()
      
      output$schedule <- renderDT({
        schedule[[1]]
      })
       
    }
  )
}

