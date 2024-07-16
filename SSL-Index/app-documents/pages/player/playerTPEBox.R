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

playerTPEBoxServer <- function(id, data, uid = uid, updated = updated, tpeTotal = tpeTotal, tpeBanked = tpeBanked) {
  moduleServer(
    id,
    function(input, output, session) {
      #### TOTAL TPE ####
      observe({
        tpeTotal(
          data() %>% 
            then(
              onFulfilled = function(data){
                data$tpe
              }
            )
        ) 
      }) %>% 
        bindEvent(
          updated()
        )
      
      output$tpeTotal <- renderText({
        tpeTotal()
      }) 
      
      #### REMAINING TPE ####
      observe({
        tpeBanked(
          data() %>% 
            then(
              onFulfilled = function(data){
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
              }
            )
          )
      }) %>% 
        bindEvent(
          updated()
        )
      
      output$tpeRemaining <- renderText({
        tpeBanked()
      })
      
      #### AC and TRAINING CAMP ####
      output$buttonAC <- renderUI({
        data() %>% 
          then(
            onFulfilled = function(data){
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
            }
          )
      })
      
      output$buttonTrainingCamp <- renderUI({
        data() %>% 
          then(
            onFulfilled = function(data){
              if(completedTrainingCamp(data$pid)){
                # Show no button if TC is completed
              } else {
                actionButton(
                  session$ns("trainingCamp"),
                  "Seasonal Training Camp"
                )
              }
            }
          )
      })
      
      #### OBSERVERS ####
      observe({
        data() %>% 
          then(
            onFulfilled = function(data){
              tpeEarned <- 6
              
              tpeSummary <- 
                tibble(
                  source = "Activity Check",
                  tpe = tpeEarned
                )
              
              tpeLog(uid = uid, pid = data$pid, tpe = tpeSummary)
              
              shinyjs::disable(session$ns("activityCheck"))
              
              updateTPE(pid = data$pid, tpe = tpeSummary)
              
              showToast(type = "success", "You have successfully claimed your Activity Check for the week!")
              
              if(tpeBanked() %>% class() == "numeric"){
                tpeBanked(tpeBanked() + tpeEarned)  
              } else {
                tpeBanked() %>% 
                  then(
                    onFulfilled = function(bank){
                      tpeBanked(bank + tpeEarned)
                    }
                  )
              }
              
              if(tpeTotal() %>% class() == "numeric"){
                tpeTotal(tpeTotal() + tpeEarned)  
              } else {
                tpeTotal() %>% 
                  then(
                    onFulfilled = function(bank){
                      tpeTotal(bank + tpeEarned)
                    }
                  )
              }
              
              updated(updated() + 1)
            }
          )
      }) %>% 
        bindEvent(
          input$activityCheck,
          ignoreInit = TRUE,
          once = TRUE
        )
      
      observe({
        data() %>% 
          then(
            onFulfilled = function(data){
              class <- 
                data$class %>% 
                str_extract_all(pattern = "[0-9]+") %>% 
                as.numeric()
              
              age <- currentSeason$season - class
              
              tpeSummary <- 
                tibble(
                  source = paste("S", currentSeason$season, " Training Camp", sep = ""),
                  tpe = case_when(
                    age <= 2 ~ 40,
                    age <= 5 ~ 30,
                    age <= 8 ~ 20,
                    TRUE ~ 10
                  )
                )
              
              tpeLog(uid = uid, pid = data$pid, tpe = tpeSummary)
              
              shinyjs::disable(session$ns("trainingCamp"))
              
              updateTPE(pid = data$pid, tpe = tpeSummary)
              
              showToast(type = "success", "You have successfully claimed your Training Camp for the season.")
              
              if(tpeBanked() %>% class() == "numeric"){
                tpeBanked(tpeBanked() + tpeSummary$tpe)  
              } else {
                tpeBanked() %>% 
                  then(
                    onFulfilled = function(bank){
                      tpeBanked(bank + tpeSummary$tpe)
                    }
                  )
              }
              
              if(tpeTotal() %>% class() == "numeric"){
                tpeTotal(tpeTotal() + tpeSummary$tpe)  
              } else {
                tpeTotal() %>% 
                  then(
                    onFulfilled = function(bank){
                      tpeTotal(bank + tpeSummary$tpe)
                    }
                  )
              }
              
              updated(updated() + 1)
            }
          )
        
      }) %>% 
        bindEvent(
          input$trainingCamp,
          ignoreInit = TRUE,
          once = TRUE
        )
      
    }
  )
}