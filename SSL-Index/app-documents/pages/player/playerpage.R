playerPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width = 12,
      fluidRow(
        playerInfoBoxUI(id = ns("playerInfo"))
      ),
      fluidRow(
        playerTPEBoxUI(id = ns("playerBuild")),
        playerOverviewBoxUI(id = ns("playerBuild")),
        box(
          title = "Update Player", collapsible = FALSE, width = NULL,
          fluidRow(
            column(width = 12,
                   uiOutput(ns("selectPlayer"))
            )
          ) %>% 
            div(align = "center", id = ns("playerSelector"))  %>%
            hidden(),
          fluidRow(
            column(
              width = 4,
              h4("Physical " %>% strong(), align = "center"),
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
              h4("Mental " %>% strong(), align = "center"),
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
              h4("Technical " %>% strong(), align = "center"),
              uiOutput(ns("technical"))
            )
          ),
          fluidRow(
            box(
              title = "Player Traits and Positions",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              status = "info",
              width = NULL,
              fluidRow(
                column(
                  width = 8,
                  offset = 2,
                  uiOutput(
                    outputId = ns("positionSelector")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 10,
                  offset = 1,
                  uiOutput(
                    outputId = ns("traitSelector")
                  )
                )
              )
            ) %>%
              column(width = 12)
          ) %>%
            div(id = ns("outfieldExtras")) %>%
            hidden(),
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
              uiOutput(ns("verifyButton"))
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

playerPageServer <- function(id, uid, parent) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #### REACTIVES ####
      updating <- 
        reactiveVal({FALSE})
      
      redistributing <- 
        reactiveVal({FALSE})
      
      rerolling <- 
        reactiveVal({FALSE})
      
      updated <- reactiveVal({0})
      
      playerData <- 
        reactive({
          getPlayerDataAsync(uid = uid)
        }) %>% 
        bindEvent(
          updated()
        )
      
      
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
      # Player selector output
      output$selectPlayer <- renderUI({
        
        playerData() %>% 
          then(
            onFulfilled = function(data){
              if(data$pos_gk == 20){
                radioButtons(
                  inputId = session$ns("playerType"),
                  label = "Outfield or Goalkeeper",
                  choices = c("Outfield", "Goalkeeper"),
                  selected = "Goalkeeper"
                )
              } else {
                radioButtons(
                  inputId = session$ns("playerType"),
                  label = "Outfield or Goalkeeper",
                  choices = c("Outfield", "Goalkeeper"),
                  selected = "Outfield"
                )
              }
            }
          )
        
      })
      
      # Dynamic technical attributes 
      output$technical <- renderUI({
        playerData() %>% 
          then(
            onFulfilled = function(data){ 
              if(input$playerType %>% is.null()){
                if(data$pos_gk != 20){
                  c(
                    "Corners", "Crossing", "Dribbling", "Finishing", "First Touch",
                    "Free Kick", "Heading", "Long Shots", "Long Throws", "Marking",
                    "Passing", "Penalty Taking", "Tackling", "Technique"
                  ) %>% 
                    str_to_lower() %>% 
                    map(
                      .x = .,
                      .f = 
                        ~ attributeInput(ns = session$ns, name = .x, value = data[,.x])
                    )
                } else {
                  attributes %>% 
                    filter(
                      group %in% c("Goalkeeper", "Technical"),
                      keeper == "TRUE"
                    ) %>% 
                    select(
                      attribute
                    ) %>% 
                    unlist() %>% 
                    unname() %>% 
                    str_to_lower() %>% 
                    map(
                      .x = .,
                      .f = 
                        ~ attributeInput(ns = session$ns, name = .x, value = data[,.])
                    )
                }
              } else {
                if(input$playerType == "Outfield"){
                  c(
                    "Corners", "Crossing", "Dribbling", "Finishing", "First Touch",
                    "Free Kick", "Heading", "Long Shots", "Long Throws", "Marking",
                    "Passing", "Penalty Taking", "Tackling", "Technique"
                  ) %>% 
                    str_to_lower() %>% 
                    map(
                      .x = .,
                      .f = 
                        ~ attributeInput(ns = session$ns, name = .x, value = data[,.x])
                    )
                } else {
                  attributes %>% 
                    filter(
                      group %in% c("Goalkeeper", "Technical"),
                      keeper == "TRUE"
                    ) %>% 
                    select(
                      attribute
                    ) %>% 
                    unlist() %>% 
                    unname() %>% 
                    str_to_lower() %>% 
                    map(
                      .x = .,
                      .f = 
                        ~ attributeInput(ns = session$ns, name = .x, value = data[,.])
                    )
                }
              }
              
            }
          )
      })
      
      
      # Dynamic UI for position selector
      output$positionSelector <- renderUI({
        playerData() %>% 
          then(
            onFulfilled = function(data){
              
              posPrim <- positions[names(positions) %in% (data %>% 
                                     select(pos_st:pos_gk) %>% 
                                     pivot_longer(everything(), names_to = "pos", values_to = "xp") %>%
                                     filter(xp == 20) %>% 
                                     mutate(pos = str_remove_all(pos, pattern = "pos_") %>% str_to_upper()) %>% 
                                       select(pos) %>% unlist())
              ]
              
              posSec <- positions[names(positions) %in% (data %>% 
                                    select(pos_st:pos_gk) %>% 
                                    pivot_longer(everything(), names_to = "pos", values_to = "xp") %>%
                                    filter(xp <= 15, xp >= 10) %>% 
                                    mutate(pos = str_remove_all(pos, pattern = "pos_") %>% str_to_upper()) %>% 
                                      select(pos) %>% unlist())
              ]
              
              posRem <- positions[!(positions %in% c(posPrim, posSec))]
              
              tagList(
                bucket_list(
                  header = paste("You may select", length(posPrim), "primary position(s) and ", length(posSec), "secondary positions."),
                  group_name = session$ns("pos"),
                  orientation = "horizontal",
                  add_rank_list(
                    text = withTooltip("Select one primary position:", "Converts to 20 positional experience in the position"),
                    labels = posPrim,
                    input_id = session$ns("primary")
                  ),
                  add_rank_list(
                    text = withTooltip("Select two secondary positions:", "Converts to 15 positional experience in the position"),
                    labels = posSec,
                    input_id = session$ns("secondary")
                  ),
                  add_rank_list(
                    text = "Drag from here",
                    labels = posRem,
                    input_id = session$ns("unusedPositions")
                  )
                )
              )
            }
          )
      })
      
      # Dynamic UI for trait selectors
      output$traitSelector <- renderUI({
        playerData() %>% 
          then(
            onFulfilled = function(data){
              currentTraits <- data$traits %>% str_split(pattern = " \\\\ ", simplify = TRUE) %>% unlist()
              nrTraits <- length(currentTraits)
              
              tagList(
                checkboxGroupInput(
                  session$ns("traits"), 
                  paste("Select", nrTraits,"traits:"), 
                  choices = traits %>% unlist(use.names = FALSE), 
                  selected = currentTraits
                  ) %>% 
                  div(class = "multicol")
              )
            }
          )
      })
      
      
      ## All the cost outputs
      editableAttributes %>% 
        lapply(
          X = .,
          FUN = function(x){
            output[[paste0("cost", x)]] <- 
              renderUI({
                if(session$input[[x]] %>% is.na() | session$input[[x]] < 5 | session$input[[x]] > 20){
                  paste(
                    "You need to input a value between 5 and 20."
                  )
                } else {
                  nextcost <- tpeCost[tpeCost$value == (session$input[[x]] + 1), "sinCost"]
                  
                  if(length(nextcost) == 0) nextcost <- ""
                  
                  paste(
                    "Next: ",
                    nextcost,
                    "Total: ",
                    tpeCost[tpeCost$value == session$input[[x]], "cumCost"]
                  )
                }
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
      
      
      
      output$verifyButton <- renderUI({
        actionButton(
          inputId = session$ns("verifyUpdate"),
          if_else(updating(), "Update", if_else(redistributing(), "Redistribute", "Reroll"))
        )
      })
      
      #### OBSERVERS ####
      
      # Observer for the player boxes
      
      observe({
        playerData() %>% 
          then(
            onFulfilled = function(value) {
              playerInfoBoxServer(id = "playerInfo", pid = value$pid)
              tpe <- playerTPEBoxServer(id = "playerBuild", data = value)
              playerOverviewBoxServer(id = "playerBuild", data = value, tpe = tpe)
            },
            onRejected = function(reason) {
              showToast("error", "An error occurred when loading your player. Please notify the BoD.")
            }
          )
      }) %>% 
        bindEvent(playerData(), ignoreNULL = FALSE)
      
      # Updates the banked tpe when changing attributes
      observe({
        if(updating() | redistributing() | rerolling()){
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
          rerolling(),
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
      
      # Fixes moving away from locked 20 in stamina and natural fitness
      observe({
        if(updating() | redistributing() | rerolling()){
          c("natural fitness", "stamina") %>% 
          map(
            .x = .,
            .f = function(x){
              if(input[[x]] %>% is.null()){
                updateNumericInput(
                  session = session,
                  inputId = x %>% stringr::str_to_title() %>% str_remove_all(pattern = " "),
                  value = 20
                ) 
              } else {
                updateNumericInput(
                  session = session,
                  inputId = x %>% stringr::str_to_title() %>% str_remove_all(pattern = " "),
                  value = if_else(input[[x]] != 20, 20, input[[x]])
                )  
              }
            }
          )
        }
      }) %>% 
        bindEvent(
          # Changes in only Natural Fitness and Stamina to lock it at 20
          {
            c("Natural Fitness", "Stamina") %>% 
              stringr::str_to_title() %>% 
              str_remove_all(pattern = " ") %>% 
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
              select(!c(`natural fitness`, stamina)) %>% 
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
              select(!c(`natural fitness`, stamina)) %>% 
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
              select(!c(`natural fitness`, stamina)) %>% 
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
              select(!c(`natural fitness`, stamina)) %>% 
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
        shinyjs::hide("attributeOverview")
        shinyjs::show("attributeUpdate")
        shinyjs::hide("tpeButtons")
        shinyjs::show("buttonsUpdating")
        shinyjs::show("outfieldExtras")
        shinyjs::show("playerSelector")
        
        redistributing(TRUE)
        
        showModal(
          modalDialog(
            "You may only submit one redistribution in your career. If you only want to update, please click the Back button.",
            title="Redistribution limit!",
            footer = modalButton("I understand!"),
            easyClose = FALSE
          )
        )
        
        promise_all(
          data1 = playerData(), 
          data2 = playerData()
        ) %...>% 
          with({
            data1 %>% 
              select(acceleration:throwing) %>% 
              select(!c(`natural fitness`, stamina)) %>% 
              colnames() %>% 
              map(
                .x = .,
                .f = function(x){
                  updateNumericInput(
                    session = session,
                    inputId = x %>% stringr::str_to_title() %>% str_remove_all(pattern = " "),
                    value = data2[, x],
                    min = 5,
                    max = 20
                  )
                }
              )
            
            data1 %>% 
              select(acceleration:throwing) %>% 
              select(!c(`natural fitness`, stamina)) %>% 
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
          input$goToRedist,
          ignoreInit = TRUE
        )
      
      observe({
        shinyjs::hide("attributeOverview")
        shinyjs::show("attributeUpdate")
        shinyjs::hide("tpeButtons")
        shinyjs::show("buttonsUpdating")
        shinyjs::show("outfieldExtras")
        shinyjs::show("playerSelector")
        
        rerolling(TRUE)
        
        showModal(
          modalDialog(
            "You may only submit one reroll in your career. If you only want to update, please click the Back button.",
            title="Reroll limit!",
            footer = modalButton("I understand!"),
            easyClose = FALSE
          )
        )
        
        promise_all(
          data1 = playerData(), 
          data2 = playerData()
        ) %...>% 
          with({
            data1 %>% 
              select(acceleration:throwing) %>% 
              select(!c(`natural fitness`, stamina)) %>% 
              colnames() %>% 
              map(
                .x = .,
                .f = function(x){
                  updateNumericInput(
                    session = session,
                    inputId = x %>% stringr::str_to_title() %>% str_remove_all(pattern = " "),
                    value = 5,
                    min = 5,
                    max = 20
                  )
                }
              )
            
            data1 %>% 
              select(acceleration:throwing) %>% 
              select(!c(`natural fitness`, stamina)) %>% 
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
          input$goToReroll,
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
            } else if(any(editableAttributes %>% sapply(X = ., FUN = function(att){input[[att]] > 20 | input[[att]] < 5}, simplify = TRUE))){
              showToast("error", "One or more of your attributes are lower than 5 or higher than 20, which exceeds the allowed range of attribute values.")
            } else {
              update <- updateSummary(current = current, inputs = input) %>% 
                left_join(
                  tpeCost %>% 
                    select(value, cumCost),
                  by = c("old" = "value")
                ) %>% 
                left_join(
                  tpeCost %>% 
                    select(value, cumCost),
                  by = c("new" = "value"),
                  suffix = c("_old", "_new")
                ) %>% 
                mutate(
                  change = cumCost_old - cumCost_new
                ) 
              
              changes <- 
                update %>% 
                mutate(
                  direction = sign(change)
                ) %>% 
                group_by(direction) %>% 
                summarize(
                  tpeChange = sum(change) %>% abs()
                ) %>% 
                ungroup()
              
              if(nrow(update) > 0){
                if(!(redistributing() | rerolling()) & any((update$old - update$new) > 0) ){
                  showToast("error", paste("You cannot reduce attributes in a regular update.",
                            paste("Return ", paste0(update$attribute[(update$old - update$new) > 0], collapse = ", "), " to their original values.")))
                # Checks for the total sum of reduced attributes
                } else if(redistributing()){
                  if(input$playerType == "Outfield"){
                    posPrim <- positions[names(positions) %in% (current %>% 
                                                                  select(pos_st:pos_gk) %>% 
                                                                  pivot_longer(everything(), names_to = "pos", values_to = "xp") %>%
                                                                  filter(xp == 20) %>% 
                                                                  mutate(pos = str_remove_all(pos, pattern = "pos_") %>% str_to_upper()) %>% 
                                                                  select(pos) %>% unlist())
                    ]
                    
                    posSec <- positions[names(positions) %in% (current %>% 
                                                                 select(pos_st:pos_gk) %>% 
                                                                 pivot_longer(everything(), names_to = "pos", values_to = "xp") %>%
                                                                 filter(xp <= 15, xp >= 10) %>% 
                                                                 mutate(pos = str_remove_all(pos, pattern = "pos_") %>% str_to_upper()) %>% 
                                                                 select(pos) %>% unlist())
                    ]
                    
                    currentTraits <- current$traits %>% str_split(pattern = " \\\\ ", simplify = TRUE) %>% unlist()
                    nrTraits <- length(currentTraits)
                    
                    if(length(input$primary) != length(posPrim) | length(input$secondary) != length(posSec)){
                      showToast("error", "Your primary and/or secondary positions does not match with what you are allowed to select.")
                    } else if(input$traits %>% length() != nrTraits) {
                      showToast("error", "You have selected the wrong number of traits.")
                    } else if((changes %>% filter(direction == 1) %>% select(tpeChange) > 100)){
                      showToast("error", "You have removed more than the allowed 100 TPE in the redistribution.")  
                    } else {
                      update <- 
                        update %>% 
                        add_row(
                          attribute = "traits",
                          old = current$traits,
                          new = paste0(input$traits, collapse = " \\ ")
                        )
                      
                      modalVerify(update, session = session)
                    }
                  } else {
                    if((changes %>% filter(direction == 1) %>% select(tpeChange) > 100)){
                      showToast("error", "You have removed more than the allowed 100 TPE in the redistribution.")  
                    } else {
                      modalVerify(update, session = session)
                    }
                  }
                } else if(rerolling()){
                  if(input$playerType == "Outfield"){
                    posPrim <- positions[names(positions) %in% (current %>% 
                                                                  select(pos_st:pos_gk) %>% 
                                                                  pivot_longer(everything(), names_to = "pos", values_to = "xp") %>%
                                                                  filter(xp == 20) %>% 
                                                                  mutate(pos = str_remove_all(pos, pattern = "pos_") %>% str_to_upper()) %>% 
                                                                  select(pos) %>% unlist())
                    ]
                    
                    posSec <- positions[names(positions) %in% (current %>% 
                                                                 select(pos_st:pos_gk) %>% 
                                                                 pivot_longer(everything(), names_to = "pos", values_to = "xp") %>%
                                                                 filter(xp <= 15, xp >= 10) %>% 
                                                                 mutate(pos = str_remove_all(pos, pattern = "pos_") %>% str_to_upper()) %>% 
                                                                 select(pos) %>% unlist())
                    ]
                    
                    currentTraits <- current$traits %>% str_split(pattern = " \\\\ ", simplify = TRUE) %>% unlist()
                    nrTraits <- length(currentTraits)
                    
                    if(length(input$primary) != length(posPrim) | length(input$secondary) != length(posSec)){
                      showToast("error", "Your primary and/or secondary positions does not match with what you are allowed to select.")
                    } else if(input$traits %>% length() != nrTraits) {
                      showToast("error", "You have selected the wrong number of traits.")
                    } else {
                      update <- 
                        update %>% 
                        add_row(
                          attribute = "traits",
                          old = current$traits,
                          new = paste0(input$traits, collapse = " \\ ")
                        )
                      
                      modalVerify(update, session = session)                    
                    } 
                  } else {
                    modalVerify(update, session = session)
                  }
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
        if(input$playerType == "Outfield"){
          shinyjs::show("outfieldExtras")
        } else {
          shinyjs::hide("outfieldExtras")
        }
      }) %>% 
        bindEvent(
          input$playerType,
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
            } else if(any(editableAttributes %>% sapply(X = ., FUN = function(att){input[[att]] > 20 | input[[att]] < 5}, simplify = TRUE))){
              showToast("error", "One or more of your attributes are lower than 5 or higher than 20, which exceeds the range of attributes we allow.")
            } else if(bank > 24){
              # Error shown if user has regressed too much
              showToast("error", "You have regressed too much. You may only remove up to 24 TPE more than the required regressed TPE.") 
            } else {
              update <- updateSummary(current = current, inputs = input)
              
              if(nrow(update) > 0){
                if(any((update$old - update$new) < 0)){
                  showToast("error", paste("You cannot increase attributes in a regression update.",
                                           paste("Return ", paste0(update$attribute[(update$old - update$new) < 0], collapse = ", "), " to their original values.")))
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
            
            updateBuild(pid = current$pid, updates = update, bank = bank)
            
            if(redistributing()){
              completeRedistribution(current$pid)
            }
            
            if(rerolling()){
              completeReroll(current$pid)
            }
            
            updated(updated() + 1)
            
            updating(FALSE)
            redistributing(FALSE)
            rerolling(FALSE)
            
            shinyjs::show("attributeOverview")
            shinyjs::hide("attributeUpdate")
            shinyjs::hide("buttonsUpdating")
            shinyjs::hide("buttonsRegression")
            shinyjs::show("tpeButtons")
            shinyjs::hide("outfieldExtras")
            shinyjs::hide("playerSelector")
            
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
        
        shinyjs::show("attributeOverview")
        shinyjs::hide("attributeUpdate")
        shinyjs::hide("buttonsUpdating")
        shinyjs::hide("buttonsRegression")
        shinyjs::show("tpeButtons")
        
        tpeBanked(playerData() %>% 
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
                    ))
        
        updating(FALSE)
        redistributing(FALSE)
        rerolling(FALSE)
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
              
              showToast(type = "success", "You have successfully claimed your Activity Check for the week!")
              
              tpeBanked(
                tpeBanked() %>% 
                  then(
                    onFulfilled = function(res){
                      res + 6
                    }
                  )
              )  
              
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
              
              showToast(type = "success", "You have successfully claimed your Training Camp for the season.")
              
              tpeBanked(
                tpeBanked() %>% 
                  then(
                    onFulfilled = function(res){
                      res + tpeSummary$tpe
                    }
                  )
              )    
              
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
        removeModal()
        
        playerData() %>%
          then(
            onFulfilled = function(value){
              completeRetirement(pid = value$pid)
              
              showToast(type = "success", "You have now retired your player.")
              
              updateTabItems(parent, "tabs", "welcome")
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