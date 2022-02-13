
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
              unique(
                playerData$Class
              ) %>% 
              str_extract(pattern = "[0-9]+") %>% 
              unname() %>% 
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
      
      standings <- reactive({
        
        url <- 
          paste(
            "https://raw.githack.com/canadice/ssl-index/main/SSL-Index/data/S",
            input$season,
            "_standings.html",
            sep = ""
          )
        
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
          ) %>% 
          left_join(
            teamInfo %>% 
              select(
                team, 
                color.primary,
                color.secondary
              ),
            by = c("Team" = "team")
          ) 
        
      })
      
      output$standings <- renderDT({
        datatable(
          standings(), 
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

