playerOverviewBoxUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = "Player Overview", collapsible = TRUE, width = NULL,
      fluidRow(
        column(
          width = 12,
          align = "right", 
          dropMenu(
            actionButton("go0", label = NULL, icon = icon("chevron-down")),
            uiOutput(
              outputId = ns("buttonRegression")
            ),
            uiOutput(
              outputId = ns("buttonUpdate")
            ),
            uiOutput(
              outputId = ns("buttonReroll")
            ),
            uiOutput(
              outputId = ns("buttonRedistribution")
            ),
            actionButton(ns("goToRetire"), label = "Retire"),
            placement = "left-end"
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
        if(tpeBanked() %>% class() == "numeric"){
          if(tpeBanked() < 0) {
            actionButton(
              inputId = session$ns("goToRegression"),
              "Regress"
            )
          } else {
            actionButton(
              inputId = session$ns("goToRegression"),
              "Regress",
              disabled = ""
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
                    "Regress",
                    disabled = ""
                  )
                }
              }
            )
        }
      })
      
      output$buttonUpdate <- renderUI({
        if(tpeBanked() %>% class() == "numeric"){
          if(tpeBanked() > 0) {
            actionButton(
              inputId = session$ns("goToUpdate"),
              "Update"
            )
          } else {
            actionButton(
              inputId = session$ns("goToUpdate"),
              "Update",
              disabled = ""
            )
          }
        } else {
          tpeBanked() %>% 
            then(
              onFulfilled = function(bank){
                if(bank > 0) {
                  actionButton(
                    inputId = session$ns("goToUpdate"),
                    "Update"
                  )
                } else {
                  actionButton(
                    inputId = session$ns("goToUpdate"),
                    "Update",
                    disabled = ""
                  )
                }
              }
            )
        }
      })
      
      output$buttonReroll <- renderUI({
        data() %>% 
          then(
            onFulfilled = function(data){
              # Rerolls can be made by users in their first two seasons in the SSL League proper
              check <- 
                ((data$class %>% str_extract(pattern = "[0-9]+") %>% as.numeric()) > (currentSeason$season - 2)) & 
                (data$rerollused == 0)
              
              if(check) {
                actionButton(
                  inputId = session$ns("goToReroll"),
                  "Reroll"
                )
              } else {
                # SHOW NOTHING
                
                # actionButton(
                #   inputId = session$ns("goToReroll"),
                #   "Reroll",
                #   disabled = ""
                # )
              }
            }
          )
      })
      
      output$buttonRedistribution <- renderUI({
        data() %>% 
          then(
            onFulfilled = function(data){
              # Redistributions can be made by users in their first season in the SSL League proper
              check <- 
                ((data$class %>% str_extract(pattern = "[0-9]+") %>% as.numeric()) > (currentSeason$season - 1)) & 
                (data$redistused == 0)
              
              if(check) {
                actionButton(
                  inputId = session$ns("goToRedist"),
                  "Redistribute"
                )
              } else {
                # SHOW NOTHING
                
                # actionButton(
                #   inputId = session$ns("goToRedist"),
                #   "Redistribute",
                #   disabled = ""
                # )
              }
            }
          )
      })
      
      #### TABLES ####
      output$playerOverview <- renderUI({
        data() %>% 
          then(
            onFulfilled = function(data){
              visData <- 
                data %>% 
                select(acceleration:throwing) %>% 
                select(where(~ !is.na(.x))) %>% 
                pivot_longer(
                  cols = everything(),
                  values_to = "Value",
                  names_to = "Attribute"
                ) %>% 
                mutate(
                  Attribute = str_to_title(Attribute)
                ) %>% 
                left_join(
                  attributes,
                  by = c("Attribute" = "attribute") 
                ) %>% 
                mutate(
                  Attribute = factor(Attribute, levels = sort(Attribute %>% unique(), decreasing = TRUE)),
                  group = factor(group, levels = c("Physical", "Mental", "Technical", "Goalkeeper")),
                  ValueFill = case_when(
                    Value >= 15 ~ 1,
                    Value >= 10 ~ 2,
                    TRUE ~ 3
                  ) %>% factor()
                ) %>% 
                {
                  if(data$pos_gk == 20){
                    filter(
                      ., 
                      (group %in% c("Goalkeeper", "Technical") & keeper == "TRUE") | (group %in% c("Physical", "Mental"))
                    )
                  } else {
                    filter(
                      .,
                      group %in% c("Physical", "Mental", "Technical")
                    )
                  }
                }
              
              map(.x = visData$group,
                  .f = function(chosenGroup){
                    output[[chosenGroup]] <- renderReactable({
                      temp <- 
                        visData %>% 
                        filter(
                          group == chosenGroup
                        )
                      
                      temp %>% 
                        select(Attribute, Value) %>% 
                        reactable(
                          defaultColDef = colDef(
                            minWidth = 50,
                            style = function(value, index){
                              color <- if_else(temp$ValueFill[index] == 1, "#66B38C", if_else(temp$ValueFill[index] == 2, "#F5D17E", "#D96F68"))
                              # color <- if_else(temp$ValueFill[index] == 1, "#008450", if_else(temp$ValueFill[index] == 2, "#EFB700", "#B81D13")) 
                              list(background = color, color = "black")
                            }
                          ),
                          pagination = FALSE,
                          sortable = FALSE
                        )
                    })
                  })
                
              map(.x = visData$group %>% unique(),
                  .f = function(group){
                    column(width = 12 / length(visData$group %>% unique()),
                           h4(group),
                           reactableOutput(session$ns(group))
                           )
                  }) %>% 
                tagList()
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
              
              showToast(type = "success", "You have now retired your player.")
              
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