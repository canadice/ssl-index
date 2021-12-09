
############################################################################
############################################################################
###                                                                      ###
###                 POSITION TRACKER CREATED FOR THE SHL                 ###
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
          "The pitch on the right shows the number of players that have at least the threshold value of XP in a given position." %>% h5(),
          br(),
          sliderInput(
            inputId = ns("xpThreshold"),
            label = "Select XP threshold<br>(at least this xp)" %>% HTML(),
            min = 0,
            max = 20,
            value = 15,
            step = 1
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
        playerData %>% 
          select(
            Striker:Goalkeeper
          ) %>% 
          mutate(
            across(
              .fns = function(x) x >= input$xpThreshold
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

