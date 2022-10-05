
############################################################################
############################################################################
###                                                                      ###
###                      COMPARE PLAYERS IN THE SSL                      ###
###                                                                      ###
############################################################################
############################################################################


### UI module for player similarities using MDS
trackerTPEUI <- function(id){
  
  ## Creating the namespacing function for all IDs
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 2,
          box(
            title = "Information",
            status = "info",
            solidHeader = TRUE,
            width = NULL,
            selectInput(
              inputId = ns("class"),
              label = "Select Draft Class",
              choices = c(
                "ALL",
                unique(playerData$Class) %>% sort(decreasing = TRUE))
            )
          )
        ),
        column(
          width = 10,
          box(
            title = "Tracker",
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            DT::DTOutput(
              outputId = ns("tableTPE")
            )
          )
        )
      )
    )
  )
}

## Backend module for player similarities
trackerTPESERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){
      
      ## js function for automatic reranking
      js <- c(
        "table.on('draw.dt', function(){",
        "  var PageInfo = table.page.info();",
        "  table.column(0, {page: 'current'}).nodes().each(function(cell,i){", 
        "    cell.innerHTML = i + 1 + PageInfo.start;",
        "  });",
        "})")
      
      ## Loads selected data for TPE Tracker
      currentTPEData <- reactive({
        if(input$class != "ALL"){
          playerData <- 
            playerData %>% 
            filter(
              Class == input$class
            )  
        }
        
        playerData %>% 
          select(
            Name,
            Username,
            Class,
            Team,
            `Preferred Position`,
            TPE,
            # `Applied TPE` = TPE - `TPE Available`,
            Active
          ) %>% 
          left_join(
            teamInfo %>% 
              select(
                team, 
                color_primary,
                color_secondary
              ),
            by = c("Team" = "team")
          )
        
      })
      
      ## TPE Tracker for different classes
      output$tableTPE <- renderDT({
        datatable(
          currentTPEData() %>% 
            arrange(
              -TPE
            ), 
          callback = JS(js),
          style = "bootstrap",
          class = 'compact cell-border stripe',
          rownames = TRUE,
          escape = FALSE,
          options = 
            list(
              ordering = TRUE, 
              ## Sets a scroller for the rows
              scrollX = '800px',
              scrollY = '550px',
              ## Sets size of rows shown
              scrollCollapse = TRUE,
              paging = FALSE,
              dom = 'ft',
              columnDefs = 
                list(
                  list(
                    targets = 8:9,
                    visible = FALSE
                  )
                )
            )
        ) %>% 
          formatStyle(
            columns = 0:7,
            valueColumns = "color_primary",
            backgroundColor = 
              styleEqual(
                sort(unique(teamInfo$color_primary)), 
                sort(unique(teamInfo$color_primary))
              )
          ) %>% 
          formatStyle(
            columns = 0:7,
            valueColumns = "color_secondary",
            color = 
              styleEqual(
                sort(unique(teamInfo$color_secondary)), 
                sort(unique(teamInfo$color_secondary))
              )
          )
      })
    }
  )
}

