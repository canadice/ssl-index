positionTrackerUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      h2("Position Tracker"),
      column(width = 4,
             p("The pitch shows the number of players that has the position as one of their Primary/Secondary positions."),
             br(),
             radioButtons(inputId = ns("activeStatus"),label = "Show only active players?",choices = c("Yes", "No"),selected = "Yes")),
      column(width = 8,imageOutput(outputId = ns("fieldImage"),height = 600))
    )
  )
}

positionTrackerServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      #### POSITION TRACKER OUTPUT ####
      trackerData <- reactive({
        getAllPlayerPositions() |> 
          then(
            onFulfilled = function(data){
              if(input$activeStatus == "Yes"){
                data <- 
                  data |> 
                  filter(
                    status == "Active"
                  )  
              }
              
              data |> 
                pivot_longer(
                  !status,
                  names_to = "posExp",
                  values_to = "Value"
                ) |> 
                cbind(
                  positionalCoord,
                  .
                ) |> 
                filter(
                  Value != 0
                ) |> 
                group_by(position) |> 
                summarize(
                  x = mean(x),
                  y = mean(y),
                  primary = sum(Value == 20),
                  secondary = sum(Value == 15)
                ) |> 
                ungroup()
            }
          )
        
        
        
        
      })
      
      output$fieldImage <- renderImage({
        trackerData() |> 
          then(
            onFulfilled = function(data){
              base <- pitch |> image_ggplot()
              
              p <- 
                base + 
                geom_text(
                  mapping = aes(x = x, y = y),
                  data = data,
                  label = data$primary,
                  nudge_x = -35,
                  size = 8,
                  fontface = "bold"
                ) + 
                geom_text(
                  mapping = aes(x = x, y = y),
                  data = data,
                  label = "/",
                  size = 8,
                  fontface = "bold"
                ) + 
                geom_text(
                  mapping = aes(x = x, y = y),
                  data = data,
                  label = data$secondary,
                  nudge_x = 35,
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
                  label = positionalCoord$position,
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
                card |> 
                image_crop(geometry = "480x600+160") |> 
                image_write(tempfile(fileext = "png"), format = "png")
              
              return(
                list(
                  src = tempImage, 
                  contentType = "image/png"
                )
              )
            }
          )
        
        
      },
      deleteFile = TRUE
      )
    }
  )
}
