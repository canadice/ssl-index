playerPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 8,
        textOutput(
          outputId = ns("playerInfo")
        ) %>% 
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
        )
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
              inputId = ns("resetUpdate"),
              "Reset"
            ),
            actionButton(
              inputId = ns("verifyUpdate"),
              "Update"
            )
          )
        )
      ) %>% 
        div(id = ns("attributeUpdate")) %>% 
        hidden(),
      box(
        title = "Regress Player", collapsible = FALSE, width = NULL,
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
      ) %>% 
        div(id = ns("attributeRegress")) %>% 
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
      )
    )
  )
}

playerPageServer <- function(id, userinfo) {
  moduleServer(
    id,
    function(input, output, session) {
      
      editableAttributes <- 
        attributes$attribute %>% 
        .[!(. %in% c("Natural Fitness", "Stamina"))] %>% 
        str_to_title() %>% 
        str_remove_all(pattern = " ") 
      
      #### REACTIVES ####
      playerData <- 
        reactive({
          getPlayerDataAsync(uid = userinfo$uid)
        })
      
      tpeTotal <- 
        reactiveVal({
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
      
      updating <- 
        reactiveVal({FALSE})
      
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
      output$playerInfo <- renderText({
        playerData() %>% 
          then(
            onFulfilled = function(value) {
              value$name
            },
            onRejected = function(reason) {
              "An error occurred."
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
        historyTPE() %...>%
          reactable()
      })
        
      output$historyUpdates <- renderReactable({
        historyUpdates() %...>%
          reactable()
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
      
      #### OBSERVERS ####
      
      # Opens updating
      observe({
        shinyjs::toggle("attributeOverview")
        shinyjs::toggle("attributeUpdate")
        
        updating(TRUE)
      }) %>% 
        bindEvent(input$goToUpdate,ignoreInit = TRUE)
      
      # Opens regression
      observe({
        shinyjs::toggle("attributeOverview")
        shinyjs::toggle("attributeRegress")
        
        updating(TRUE)
      }) %>% 
        bindEvent(input$goToRegression,ignoreInit = TRUE)
      
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
                    min = data2[, x]
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
                    max = data2[, x]
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
          input$resetUpdate,
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
              update <- 
                tibble(
                  attribute = 
                    current %>% 
                    select(acceleration:throwing) %>% 
                    select(!where(is.na)) %>% 
                    colnames() %>%
                    str_to_title(),
                  old = current %>% 
                    select(acceleration:throwing) %>% 
                    select(!where(is.na)) %>% 
                    t(),
                  new = 
                    attribute %>%
                    str_remove_all(pattern = " ") %>% 
                    sapply(
                      X = .,
                      FUN = function(x) input[[x]],
                      simplify = TRUE
                    ) %>% 
                    unlist()
                ) %>% 
                filter(old != new)
              
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
          input$verifyUpdate
        )
      
    }
  )
}