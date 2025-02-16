playerOverviewBoxUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = "Player Overview", collapsible = TRUE, width = NULL,
      fluidRow(
        column(
          width = 12,
          align = "right", 
          tagList(
            flexRow(
              tagList(
                uiOutput(
                  outputId = ns("buttonUpdate")
                ),
                uiOutput(
                  outputId = ns("buttonRegression")
                ),
                # uiOutput(
                #   outputId = ns("buttonReroll")
                # ),
                # uiOutput(
                #   outputId = ns("buttonRedistribution")
                # ),
                uiOutput(
                  outputId = ns("buttonGoToRetire")
                ),
              ),
              style = "justify-content: flex-end; gap: 8px;"
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          uiOutput(ns("playerOverview")) %>% 
            withSpinnerMedium()
        )
      )
    ) %>% 
      div(id = ns("attributeOverview"))
  )
}

playerOverviewBoxServer <- function(id, data, tpeTotal = tpeTotal, tpeBanked = tpeBanked, mainSession) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #### BUTTONS ####
      
      output$buttonRegression <- renderUI({
        if(tpeBanked() %>% class() %in% c("numeric", "integer")){
          if(tpeBanked() < 0) {
            actionButton(
              inputId = session$ns("goToRegression"),
              "Regress"
            )
          } else {
            actionButton(
              inputId = session$ns("goToRegression"),
              tippy("Regress", "You do not need to regress your player", theme = "ssl", arrow = TRUE),
              disabled = TRUE
            )
          }
        } else {
          tpeBanked() %>% 
            then(
              onFulfilled = function(bank){
                if(bank < 0) {
                  actionButton(
                    inputId = session$ns("goToRegression"),
                    "Regress"
                  )
                } else {
                  actionButton(
                    inputId = session$ns("goToRegression"),
                    tippy("Regress", "You do not need to regress your player", theme = "ssl", arrow = TRUE),
                    disabled = TRUE
                  )
                }
              }
            )
        }
      })
      
      output$buttonUpdate <- renderUI({
        if(tpeBanked() %>% class() %in% c("numeric", "integer")){
          if(tpeBanked() > 0) {
            actionButton(
              inputId = session$ns("goToUpdate"),
              "Update",
              class = "primary-button"
            )
          } else {
            actionButton(
              inputId = session$ns("goToUpdate"),
              tippy("Update", "You cannot update your player. You must first regress them.", theme = "ssl", arrow = TRUE),
              disabled = TRUE
            )
          }
        } else {
          tpeBanked() %>% 
            then(
              onFulfilled = function(bank){
                if(bank >= 0) {
                  actionButton(
                    inputId = session$ns("goToUpdate"),
                    "Update",
                    class = "primary-button"
                  )
                } else {
                  actionButton(
                    inputId = session$ns("goToUpdate"),
                    tippy("Update", "You cannot update your player. You must first regress them.", theme = "ssl", arrow = TRUE),
                    disabled = TRUE
                  )
                }
              }
            )
        }
      })
      
      observe({
        data() %>% 
          then(
            onFulfilled = function(data){
              # Rerolls can be made by users in their first two seasons in the SSL League proper
              check <- 
                ((data$class %>% str_extract(pattern = "[0-9]+") %>% as.numeric()) > (currentSeason$season - 2)) & 
                (data$rerollused == 0)
              
              if(check) {
                insertUI(
                  selector = "#yourPlayer-playerBuild-buttonGoToRetire",
                  where = "beforeBegin",
                  ui = actionButton(
                    inputId = session$ns("goToReroll"),
                    "Reroll"
                  )
                )
              }
              
              # Redistributions can be made by users in their first season in the SSL League proper
              check <- 
                ((data$class %>% str_extract(pattern = "[0-9]+") %>% as.numeric()) > (currentSeason$season - 1)) & 
                (data$redistused == 0)
              
              if(check) {
                insertUI(
                  selector = "#yourPlayer-playerBuild-buttonGoToRetire",
                  where = "beforeBegin",
                  ui = actionButton(
                    inputId = session$ns("goToRedist"),
                    "Redistribute"
                  )
                )
              }
            }
          )
      }) %>% 
        bindEvent(data())
      
      output$buttonGoToRetire <- renderUI({
        data() %>% 
          then(
            onFulfilled = function(data){
              actionButton(
                inputId = session$ns("goToRetire"),
                "Retire"
              )
            }
          )
      })
      
      #### TABLES ####
      output$playerOverview <- renderUI({
        data() %>% 
          then(
            onFulfilled = function(data){
              attributeReactable(data, session, output)
            }
          )
      })
      
      #### OBSERVERS ####
      # Retires player
      observe({
        modalRetire(session)
      }) %>% 
        bindEvent(
          input$goToRetire,
          ignoreInit = TRUE,
          once = TRUE
        )
      
      observe({
        modalRetire2(session)
      }) %>% 
        bindEvent(
          input$confirmRetirement1,
          ignoreInit = TRUE,
          once = TRUE
        )
      
      observe({
        data() %>% 
          then(
            onFulfilled = function(data){
              removeModal()
              
              completeRetirement(pid = data$pid)
              
              showToast(.options = myToastOptions,type = "success", "You have now retired your player.")
              
              updateTabItems(mainSession, "tabs", "welcome")
            }
          )
      }) %>% 
        bindEvent(
          input$confirmRetirement2,
          ignoreInit = TRUE,
          once = TRUE
        )
    }
  )
}
