
############################################################################
############################################################################
###                                                                      ###
###                 POSITION TRACKER CREATED FOR THE SSL                 ###
###                                                                      ###
############################################################################
############################################################################
 

### UI module for player similarities using MDS
trackerPositionUI <- function(id){
  
  ## Creating the namespacing function for all IDs
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 4,
          "Information" %>% h4(),
          paste("The pitch on the right shows", "the number of players" %>% strong(),
          "that have the given position as their primary or secondary position selection."
          ) %>% HTML(),
          paste("The positions within each striped border are common for teams to use 
          interchangeably, so there is a lesser need for players at both positions."),
          br(),
          br(),
          radioButtons(
            inputId = ns("xpThreshold"),
            label = "Select the type of position" %>% HTML(),
            choices = c("Primary", "Secondary"),
            selected = "Primary"
          ),
          radioButtons(
            inputId = ns("activeStatus"),
            label = "Show only active players?",
            choices = c("Yes", "No"),
            selected = "Yes"
          )
        ),
        column(
          width = 8,
          imageOutput(
            outputId = ns("fieldImage")
          )
        )
      )
    )
  )
}

## Backend module for player similarities
trackerPositionSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){
      trackerData <- reactive({
        data <- 
          playerData 
        
        if(input$activeStatus == "Yes"){
          data <- 
            data %>% 
            filter(
              Active == "Active"
            )  
        }
        
        data %>% 
          select(
            Striker:Goalkeeper
          ) %>% 
          mutate(
            across(
              .cols = everything(),
              .fns = function(x) x == if_else(input$xpThreshold == "Primary", 20, 15)
            )    
          ) %>% 
          summarize(
            across(
              .fns = sum,
              na.rm = TRUE
            )
          ) %>% 
          pivot_longer(
            where(is.numeric),
            names_to = "posExp",
            values_to = "Value"
          ) %>% 
          cbind(
            positionalCoord,
            .
          )
      }) 
        
      
      output$fieldImage <- renderImage({
        base <- pitch %>% image_ggplot()
        
        p <- 
          base + 
          geom_text(
            mapping = aes(x = x, y = y),
            data = trackerData(),
            label = trackerData()$Value,
            size = 8,
            fontface = "bold"
          ) + 
          geom_rect(
            aes(xmin = 60, xmax = 200, ymin = 100, ymax = 370),
            fill = NA,
            color = "black",
            linetype = 2
          ) + 
          geom_rect(
            aes(xmin = 550, xmax = 690, ymin = 100, ymax = 370),
            fill = NA,
            color = "black",
            linetype = 2
          ) +
          geom_rect(
            aes(xmin = 60, xmax = 200, ymin = 410, ymax = 680),
            fill = NA,
            color = "black",
            linetype = 2
          ) + 
          geom_rect(
            aes(xmin = 550, xmax = 690, ymin = 410, ymax = 680),
            fill = NA,
            color = "black",
            linetype = 2
          ) +
          geom_text(
            mapping = aes(x = x, y = y),
            data = positionalCoord,
            nudge_y = 35,
            label = positionalCoord$Position,
            size = 4,
            fontface = "italic",
            color = "#00044D"
          ) 
        
        card <- image_graph(res = 96)
        print(
          p + 
          theme(
            legend.position = "none"
          )
        )
        dev.off()
        
        tempImage <- 
          card %>% 
          image_crop(geometry = "480x600+160") %>% 
          image_write(tempfile(fileext = "png"), format = "png")
        
        return(
          list(
            src = tempImage, 
            contentType = "image/png"
            )
          )
      },
      deleteFile = TRUE
      )
    }
  )
}

