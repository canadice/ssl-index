playerAttributeBoxUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        offset = 5,
        valueBox(
          subtitle = "Available TPE" %>% 
            div(align = "center"),
          value = textOutput(ns("tpeRemaining"), inline = TRUE)  %>% 
            withSpinner30() %>% 
            div(align = "center"), 
          width = NULL
        )
      )
    ),
    fluidRow(
      column(
        width = 4,
        tagList(
          c(
            "acceleration", "agility", "balance", "jumping reach", 
            "natural fitness", "pace", "stamina", "strength"
          ) %>% 
            map(
              .x = .,
              .f = 
                ~ attributeInput(ns = ns, name = .x, value = NA)
            )
        )
      ),
      column(
        width = 4,
        c(
          "aggression", "anticipation", "bravery", "composure", "concentration", 
          "decisions", "determination", "flair", "leadership", "off the ball", 
          "positioning", "teamwork", "vision", "work rate"
        ) %>% 
          map(
            .x = .,
            .f = 
              ~ attributeInput(ns = ns, name = .x, value = NA)
          )
      ),
      column(
        width = 4,
        c(
          "Corners", "Crossing", "Dribbling", "Finishing", "First Touch",
          "Free Kick", "Heading", "Long Shots", "Long Throws", "Marking",
          "Passing", "Penalty Taking", "Tackling", "Technique", "Aerial Reach",
          "Command Of Area", "Communication", "Eccentricity", "Handling",
          "Kicking", "One On Ones", "Tendency To Punch", "Reflexes", 
          "Tendency To Rush", "Throwing"
        ) %>% 
          map(
            .x = .,
            .f = 
              ~ attributeInput(ns = ns, name = .x, value = NA)
          )
      )
    ),
    fluidRow(
      column(
        width = 12,
        align = "center", 
        style = "display: flex; justify-content: center;",
        actionButton(
          inputId = ns("resetRegression"),
          "Reset"
        ),
        actionButton(
          inputId = ns("verifyRegression"),
          "Regress"
        )
      )
    )
  )
}

playerAttributeBoxServer <- function(id, parent, pid, uid, rv) {
  moduleServer(
    id,
    function(input, output, session) {
      playerData <- 
        reactive({
          future_promise({
            readAPI("https://api.simulationsoccer.com/player/getPlayer", query = list(pid = pid))   
          })
        })
      
      output$tpeRemaining <- renderText({
        tpeBanked()
      })
      
      ## All the cost outputs
      editableAttributes %>% 
        lapply(
          X = .,
          FUN = function(x){
            output[[paste0("cost", x)]] <- 
              renderUI({
                nextcost <- tpeCost[tpeCost$value == (session$input[[x]] + 1), "sinCost"]
                
                if(length(nextcost) == 0) nextcost <- ""
                
                paste(
                  "Next: ",
                  nextcost,
                  "Total: ",
                  tpeCost[tpeCost$value == session$input[[x]], "cumCost"]
                )
                
              })
          }
        )
      
      # Updates the banked tpe when changing attributes
      tpeBanked <- reactive({
        playerData() %>% 
          then(
            onFulfilled = function(value) {
              editableAttributes %>% 
                lapply(
                  X = .,
                  FUN = function(x){
                    tpeCost[tpeCost$value == session$input[[x]], "cumCost"]
                  }
                ) %>% 
                unlist() %>% 
                sum() %>% 
                {
                  value$tpe - .
                }
            },
            onRejected = function(reason) {
              NaN
            }
          )
      }) %>% 
        bindEvent(
          # Changes in any input slider
          {
            editableAttributes %>% 
              lapply(
                X = .,
                FUN = function(x){
                  input[[x]]
                }
              )
          },
          ignoreInit = FALSE
        )
      
      # Sets up the selected build
      observe({
        promise_all(
          data1 = playerData(), 
          data2 = playerData()
        ) %...>% 
          with({
            data1 %>% 
              select(acceleration:throwing) %>% 
              colnames() %>% 
              map(
                .x = .,
                .f = function(x){
                  # print(data2)
                  
                  updateNumericInput(
                    session = session,
                    inputId = x %>% stringr::str_to_title() %>% str_remove_all(pattern = " "),
                    value = data2[, x],
                    min = 5,
                    max = if_else(
                      x %in% c("acceleration", "agility", "balance", "jumping reach", "pace", "strength"),
                      min(data2[,x], 20 - max(data2$timesregressed - 2, 0)),
                      data2[, x]
                    )
                  )
                }
              )
            
            data1 %>%
              select(acceleration:throwing) %>%
              select(
                where(is.na)
              ) %>%
              colnames() %>%
              map(
                .x = .,
                .f = function(x){
                  updateNumericInput(
                    session = session,
                    inputId = x %>% stringr::str_to_title() %>% str_remove_all(pattern = " "),
                    value = 5,
                    min = 5,
                    max = 5
                  )
                  
                  shinyjs::hide(x %>% stringr::str_to_title() %>% str_remove_all(pattern = " ") %>% paste(. ,"AttributeBox", sep = ""))
                }
              )
          })
      }) %>% 
        bindEvent(
          playerData(),
          ignoreInit = FALSE
        )
      
      # Resets the player build 
      observe({
        promise_all(
          data1 = playerData(), 
          data2 = playerData()
        ) %...>% 
          with({
            data1 %>% 
              select(acceleration:throwing) %>% 
              colnames() %>% 
              map(
                .x = .,
                .f = function(x){
                  updateNumericInput(
                    session = session,
                    inputId = x %>% stringr::str_to_title() %>% str_remove_all(pattern = " "),
                    value = data2[, x]
                  )
                }
              )
            
            data1 %>%
              select(acceleration:throwing) %>%
              select(
                where(is.na)
              ) %>%
              colnames() %>%
              map(
                .x = .,
                .f = function(x){
                  updateNumericInput(
                    session = session,
                    inputId = x %>% stringr::str_to_title() %>% str_remove_all(pattern = " "),
                    value = 5,
                    min = 5,
                    max = 5
                  )
                  
                  shinyjs::hide(x %>% stringr::str_to_title() %>% str_remove_all(pattern = " ") %>% paste(. ,"AttributeBox", sep = ""))
                }
              )
          })
      }) %>% 
        bindEvent(
          input$resetRegression,
          ignoreInit = TRUE
        )
      
      # Verifies the regression
      observe({
        promise_all(
          current = playerData(),
          bank = tpeBanked()
        ) %...>%
          with({
            if(bank < 0){
              showToast(.options = myToastOptions,"error", "You have spent too much TPE on your attributes! Reduce some of your attributes and try again.", session = parent)
            } else if(bank > 24){
              # Error shown if manager has regressed too much
              showToast(.options = myToastOptions,"error", "You have regressed too much. You may only remove up to 24 TPE more than the required regressed TPE.", session = parent) 
            } else {
              
              update <- updateSummary(current = current, inputs = input)
              
              if(nrow(update) > 0){
                if(any((update$old - update$new) < 0)){
                  showToast(.options = myToastOptions,type = "error", "You cannot have an attribute value larger than your current build.", session = parent)
                } else {
                  modalVerify(update, session = session)
                }
              } else {
                showToast(.options = myToastOptions,type = "warning", "You have not changed your build yet, there is nothing to update.", session = parent)
              }
            }
          })
      }) %>% 
        bindEvent(
          input$verifyRegression,
          ignoreInit = TRUE
        )
      
      observe({
        promise_all(
          current = playerData(),
          bank = tpeBanked()
        ) %...>%
          with({
            removeModal()
            
            update <- updateSummary(current = current, inputs = input)
            
            updateLog(uid = uid, pid = current$pid, updates = update)
            
            updateBuild(pid = current$pid, updates = update, bank = bank)
            
            showToast(.options = myToastOptions,type = "success", "The player has been regressed successfully!", session = parent)
            
            ## Sends back a value for a reactive element so that observers in parent can trigger
            rv$bank <- bank
            
          })
      }) %>% 
        bindEvent(
          input$confirmUpdate,
          ignoreInit = TRUE,
          once = TRUE
        )
      
    }
  )
}
