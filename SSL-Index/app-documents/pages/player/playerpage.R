playerPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width = 12,
      fluidRow(
        column(
          width = 8,
          uiOutput(outputId = ns("playerInfo")) %>% 
            withSpinnerMedium()
        )
      ),
      fluidRow(
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
        ),
        box(
          title = "Player Overview", collapsible = TRUE, width = NULL,
          fluidRow(
            column(
              width = 12,
              plotOutput(ns("playerOverview")) %>% 
                withSpinnerMedium()
            )
          ),
          fluidRow(
            column(
              width = 12,
              align = "center", 
              style = "display: flex; justify-content: center;",
              uiOutput(
                outputId = ns("buttonRegression")
              ) %>% 
                withSpinnerSmall(),
              uiOutput(
                outputId = ns("buttonUpdate")
              ) %>% 
                withSpinnerSmall()
            )
          )
        ) %>% 
          div(id = ns("attributeOverview")),
        box(
          title = "Update Player", collapsible = FALSE, width = NULL,
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
                inputId = ns("backUpdate"),
                "Go back"
              ),
              actionButton(
                inputId = ns("resetUpdate"),
                "Reset"
              ),
              actionButton(
                inputId = ns("verifyUpdate"),
                "Update"
              )
            )
          ) %>% 
            div(id = ns("buttonsUpdating")) %>% 
            hidden(),
          fluidRow(
            column(
              width = 12,
              align = "center", 
              style = "display: flex; justify-content: center;",
              actionButton(
                inputId = ns("backRegression"),
                "Go back"
              ),
              actionButton(
                inputId = ns("resetRegression"),
                "Reset"
              ),
              actionButton(
                inputId = ns("verifyRegression"),
                "Regress"
              )
            )
          ) %>% 
            div(id = ns("buttonsRegression")) %>% 
            hidden()
        ) %>% 
          div(id = ns("attributeUpdate")) %>% 
          hidden(),
        box(
          title = "Updating History", collapsed = TRUE, collapsible = TRUE, width = NULL,
          fluidRow(
            column(width = 12,
                   reactableOutput(ns("historyUpdates")) %>% 
                     withSpinnerMedium()
            )
          )
        ),
        box(title = "TPE History", collapsed = TRUE, collapsible = TRUE,width = NULL,
            fluidRow(
              column(
                width = 12,
                reactableOutput(ns("historyTPE"))
              )
            )
        ),
        br(),
        br()
      )
    )
  )
}

playerPageServer <- function(id, uid) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #### REACTIVES ####
      updating <- 
        reactiveVal({FALSE})
      
      updated <- reactiveVal({0})
      
      playerData <- 
        reactive({
          
          getPlayerDataAsync(uid = uid)
          
        }) %>% 
        bindEvent(
          updated()
        )
      
      tpeTotal <- 
        reactive({
          playerData() %>% 
            then(
              onFulfilled = function(value) {
                value$tpe
              },
              onRejected = function(reason) {
                NaN
              }
            )
        })
      
      tpeBanked <- 
        reactiveVal({
          playerData() %>% 
            then(
              onFulfilled = function(value) {
                value %>% 
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
                    value$tpe - .
                  }
              },
              onRejected = function(reason) {
                NaN
              }
            )
        }) 
      
      historyTPE <- 
        reactive({
          playerData() %>% 
            then(
              onFulfilled = function(value){
                getTpeHistory(value$pid)
              }
            )
        })
      
      historyUpdates <- 
        reactive({
          playerData() %>% 
            then(
              onFulfilled = function(value){
                getUpdateHistory(value$pid)
              }
            )
        })
      
      
      #### OUTPUTS ####
      output$playerInfo <- renderUI({
        playerData() %>% 
          then(
            onFulfilled = function(value) {
              playerInfoBoxUI(id = session$ns(paste("playerInfo", value$pid, sep = "-")))
              
              playerInfoBoxServer(id = session$ns(paste("playerInfo", value$pid, sep = "-")), pid = value$pid)
            },
            onRejected = function(reason) {
              showToast("error", "An error occurred when loading your player. Please notify the BoD.")
            }
          )
      })
      
      output$tpeTotal <- renderText({
        tpeTotal()
      })
      
      output$tpeRemaining <- renderText({
        tpeBanked()
      })
      
      output$playerOverview <- renderPlot({
        playerData() %>% 
          then(
            onFulfilled = function(value){
              p <- 
                value %>% 
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
                ggplot() + aes(x = Attribute, y = Value, fill = ValueFill) + 
                geom_bar(stat = "identity") +
                facet_wrap(. ~ group, scales = "free") + 
                scale_y_continuous(expand = c(0,0), limits = c(0, 20), minor_breaks = seq(0, 20, 1)) +
                scale_fill_manual(
                  guide = NULL,
                  values = c("#008450", "#EFB700", "#B81D13")
                ) +
                coord_flip() + 
                theme_bw() +
                theme(
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  axis.text = element_text(size = 14),
                  strip.text = element_text(size = 16),
                  plot.margin = unit(margin(r = 10), "pt"),
                  plot.background = element_rect(fill = "transparent", color = NA)
                ) + 
                labs(x = NULL, y = NULL)
              
              print(p)
            }
          )
      }, bg="transparent")
      
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
      
      output$historyTPE <- renderReactable({
        historyTPE() %>%
          then(
            onFulfilled = function(value){
              value %>% 
              mutate(
                time = as_datetime(time)
              ) %>% 
              reactable(
                columns = 
                  list(
                    time = colDef(format = colFormat(datetime = TRUE))
                  )
              )
            },
            onRejected = NULL
          )
          
      })
        
      output$historyUpdates <- renderReactable({
        historyUpdates() %>% 
          then(
            onFulfilled = function(value){
              value %>% 
                mutate(
                  time = as_datetime(time)
                ) %>% 
                reactable(
                  columns = 
                    list(
                      time = colDef(format = colFormat(datetime = TRUE))
                    )
                )
            },
            onRejected = NULL
          )
      })
      
      output$buttonRegression <- renderUI({
        tpeBanked() %>% 
          then(
            onFulfilled = function(value){
              if(value < 0) {
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
      })
      
      output$buttonUpdate <- renderUI({
        tpeBanked() %>% 
          then(
            onFulfilled = function(value){
              if(value > 0) {
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
      })
      
      output$buttonAC <- renderUI({
        playerData() %>% 
          then(
            onFulfilled = function(value){
              if(completedActivityCheck(value$pid)){
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
            },
            onRejected = NULL
          )
      })
      
      output$buttonTrainingCamp <- renderUI({
        playerData() %>% 
          then(
            onFulfilled = function(value){
              if(completedTrainingCamp(value$pid)){
                # Show no button if TC is completed
              } else {
                actionButton(
                  session$ns("trainingCamp"),
                  "Seasonal Training Camp"
                )
              }
            },
            onRejected = NULL
          )
      })
      
      #### OBSERVERS ####
      
      # Updates the banked tpe when changing attributes
      observe({
        if(updating()){
          tpeBanked(
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
          )
        }
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
          ignoreInit = TRUE
        )
      
      # Set attribute UI to current build and 
      # fixes minimum value when updating
      # fixes maximum value when regressing
      # only resets value when resetting
      observe({
        shinyjs::toggle("attributeOverview")
        shinyjs::toggle("attributeUpdate")
        shinyjs::toggle("tpeButtons")
        shinyjs::show("buttonsUpdating")
        
        updating(TRUE)
        
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
                    value = data2[, x],
                    min = data2[, x],
                    max = 20
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
          input$goToUpdate,
          ignoreInit = TRUE
        )
      
      observe({
        shinyjs::toggle("attributeOverview")
        shinyjs::toggle("attributeUpdate")
        shinyjs::show("buttonsRegression")
        shinyjs::toggle("tpeButtons")
        
        updating(TRUE)
        
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
                    value = data2[, x],
                    max = data2[, x],
                    min = 5
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
          input$goToRegression,
          ignoreInit = TRUE
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
          combineTriggers(input$resetUpdate, input$resetRegression),
          ignoreInit = TRUE
        )
      
      observe({
        promise_all(
          current = playerData(),
          bank = tpeBanked()
        ) %...>%
          with({
            if(bank < 0){
              modalOverdraft()
            } else {
              update <- updateSummary(current = current, inputs = input)
              
              if(nrow(update) > 0){
                if(any((update$old - update$new) > 0)){
                  modalReduction(update)
                } else {
                  modalVerify(update, session = session)
                }
              } else {
                modalNothing()
              }
            }
          })
      }) %>% 
        bindEvent(
          input$verifyUpdate,
          ignoreInit = TRUE
        )
      
      observe({
        promise_all(
          current = playerData(),
          bank = tpeBanked()
        ) %...>%
          with({
            if(bank < 0){
              showToast("error", "You have spent too much TPE on your attributes! Reduce some of your attributes and try again.")
            } else {
              update <- updateSummary(current = current, inputs = input)
              
              if(nrow(update) > 0){
                if(any((update$old - update$new) < 0)){
                  showToast(type = "error", "You cannot have an attribute value larger than your current build.")
                } else {
                  modalVerify(update, session = session)
                }
              } else {
                showToast(type = "warning", "You have not changed your build yet, there is nothing to update.")
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
            
            updateBuild(pid = current$pid, updates = update)
            
            updated(updated() + 1)
            
            updating(FALSE)
            
            shinyjs::toggle("attributeOverview")
            shinyjs::toggle("attributeUpdate")
            shinyjs::hide("buttonsUpdating")
            shinyjs::hide("buttonsRegression")
            shinyjs::toggle("tpeButtons")
            
            # print("Go back to overview from confirmation")
          })
      }) %>% 
        bindEvent(
          input$confirmUpdate,
          ignoreInit = TRUE,
          once = TRUE
        )
      
      # Adds a go back button for both updating and regression
      observe({
        # print(paste("Going back to overview", combineTriggers(input$backUpdate, input$backRegression)))
        
        shinyjs::toggle("attributeOverview")
        shinyjs::toggle("attributeUpdate")
        shinyjs::hide("buttonsUpdating")
        shinyjs::hide("buttonsRegression")
        shinyjs::toggle("tpeButtons")
        
        updating(FALSE)
      }) %>% 
        bindEvent(
          combineTriggers(input$backUpdate, input$backRegression),
          ignoreInit = TRUE
        )
      
      observe({
        playerData() %>% 
          then(
            onFulfilled = function(value){
              tpeSummary <- 
                tibble(
                  source = "Activity Check",
                  tpe = 6
                )
              
              tpeLog(uid = uid, pid = value$pid, tpe = tpeSummary)
              
              shinyjs::disable(session$ns("activityCheck"))
              
              updateTPE(pid = value$pid, tpe = tpeSummary)
              
              modalAC()
              
              updated(updated() + 1)
              
            },
            onRejected = NULL
          )
      }) %>% 
        bindEvent(
          input$activityCheck,
          ignoreInit = TRUE,
          once = TRUE
        )
      
      observe({
        playerData() %>% 
          then(
            onFulfilled = function(value){
              class <- 
                value$class %>% 
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
              
              tpeLog(uid = uid, pid = value$pid, tpe = tpeSummary)
              
              shinyjs::disable(session$ns("trainingCamp"))
              
              updateTPE(pid = value$pid, tpe = tpeSummary)
              
              modalTC(tpe = tpeSummary)
              
              updated(updated() + 1)
              
            },
            onRejected = NULL
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