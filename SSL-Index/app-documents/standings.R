
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
          width = 10,
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
      
      url <- "https://raw.githack.com/canadice/ssl-index/main/SSL-Index/data/standings.html"
      
      standings <- 
        url %>% 
        read_html() %>% 
        html_elements("table") %>% 
        html_table() %>% 
        .[[1]] %>% 
        rename(
          GP = Pld,
          W = Won,
          D = Drn,
          L = Lst,
          GF = For,
          GA = Ag
        ) %>% 
        select(
          -`Inf`,
          -Form
        )
     
      output$standings <- renderDT({
        datatable(
          standings,
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
              ## Sets color of table background
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#00044d', 'color': '#fff'});",
                "}")
            )
        )
      })
    }
  )
}

