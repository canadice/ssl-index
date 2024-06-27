playerTPEBoxUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = "TPE", collapsible = TRUE, width = NULL,
      fluidRow(
        column(
          width = 12, align = "center", style = "display: flex; justify-content: center;",
          valueBox(
            subtitle = "Total Earned TPE",
            value = textOutput(ns("tpeTotal"), inline = TRUE) %>% 
              withSpinnerSmall(),
            width = 3
          ),
          valueBox(
            subtitle = "Available TPE",
            value = textOutput(ns("tpeRemaining"), inline = TRUE) %>% 
              withSpinnerSmall(), 
            width = 3
          )
        )
      ),
      fluidRow(
        column(
          width = 12, align = "center", style = "display: flex; justify-content: center;",
          uiOutput(ns("buttonAC")),
          uiOutput(ns("buttonTrainingCamp"))
        )
      ) %>% 
        div(id = ns("tpeButtons"))
    )
  )
}

playerTPEBoxServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      #### TOTAL TPE ####
      tpeTotal <- 
        reactive({
          data$tpe
        })
      
      output$tpeTotal <- renderText({
        tpeTotal()
      })
      
      #### REMAINING TPE ####
      tpeBanked <- 
        reactiveVal({
            data %>% 
              select(acceleration:throwing) %>% 
              select(!`natural fitness` & !stamina) %>% 
              pivot_longer(
                cols = everything(),
                names_to = "attribute",
                values_to = "value"
              ) %>%
              left_join(
                tpeCost %>% 
                  select(
                    value,
                    cumCost
                  ),
                by = "value"
              ) %>% 
              select(cumCost) %>% 
              sum(na.rm = TRUE) %>% 
              {
                data$tpe - .
              }
        }) 
      
      output$tpeRemaining <- renderText({
        tpeBanked()
      })
      
      #### AC and TRAINING CAMP ####
      output$buttonAC <- renderUI({
        if(completedActivityCheck(data$pid)){
          actionButton(
            session$ns("activityCheck"),
            "Activity Check",
            disabled = ""
          )  
        } else {
          actionButton(
            session$ns("activityCheck"),
            "Activity Check"
          )
        }
      })
      
      output$buttonTrainingCamp <- renderUI({
        if(completedTrainingCamp(data$pid)){
          # Show no button if TC is completed
        } else {
          actionButton(
            session$ns("trainingCamp"),
            "Seasonal Training Camp"
          )
        }
      })
      
      return(
        list(
          total = tpeTotal(),
          banked = tpeBanked()
        )
      )
    }
  )
}