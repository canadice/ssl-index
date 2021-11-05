
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
          width = 10,
          offset = 1,
          h3("Placeholder schedule"),
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
     
      url <- "https://raw.githack.com/canadice/ssl-index/main/SSL-Index/data/schedule.html"
      
      schedule <- 
        url %>% 
        read_html() %>% 
        html_elements("table") %>% 
        html_table() %>% 
        .[[1]]
      
      output$schedule <- renderDT({
        if(length(schedule) == 0){
          stop("The current schedule file is empty. Please notify the owners.")
          # NULL
        } else {
          datatable(
            schedule,
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
                pageLength = 25,
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

