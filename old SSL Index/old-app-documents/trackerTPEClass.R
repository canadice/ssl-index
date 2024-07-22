
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
                unique(playerData$Class) %>% 
                  factor(levels = paste("S", length(.):1, sep = "")) %>% 
                  sort() %>% 
                  as.character()
                )
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
            reactableOutput(
              outputId = ns("tableTPE")
            ) %>% withSpinner()
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
      output$tableTPE <- renderReactable({
        data <- 
          currentTPEData() %>% 
          arrange(
            -TPE
          ) %>% 
          mutate(
            Rank = 1:n()
          ) %>% 
          relocate(Rank)
        
        data %>% 
          reactable(
            theme = pff(font_color = "#000"),
            pagination = FALSE,
            height = 800,
            rownames = FALSE,
            defaultColDef = 
              colDef(
                style = function(value, index){
                  list(background = data$color_primary[index], color = data$color_secondary[index])
                }
              ),
            columns = 
              list(
                Rank = 
                  colDef(
                    name = "",
                    width = 45
                  ),
                Name = 
                  colDef(
                    width = 200,
                    cell = function(value, index){
                      class <- data %>% 
                        .$Class %>% 
                        .[index]
                      
                      tagList(
                        div(value),
                        div(style = "font-size: 1.2rem", class)
                      )
                    }
                  ),
                Username = 
                  colDef(
                    width = 200
                  ),
                Class = 
                  colDef(
                    show = FALSE
                  ),
                Team = 
                  colDef(
                    width = 100,
                    cell = function(value){
                      image <- img(src = sprintf("%s.png", value), style = "height: 25px;", alt = value)  
                      
                      list <- 
                        tagList(
                          div(style = "display: inline-block; width: 25px;", image),
                          div(style = "font-size: 1.2rem", value)
                        )
                    }
                  ),
                color_primary = 
                  colDef(
                    show = FALSE
                  ),
                color_secondary = 
                  colDef(
                    show = FALSE
                  )
              )
          )
      })
    }
  )
}

