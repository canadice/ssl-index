playerPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      
      ## User information
      fluidRow(
        verbatimTextOutput(ns("user")) %>% 
          withSpinner(
            proxy.height = "20px"
          )
      ),
      
      ## TPE Information
      fluidRow(
        {
          box(
            title = "TPE",
            collapsible = TRUE,
            width = NULL,
            fluidRow(
              column(
                width = 12,
                align = "center", 
                style = "display: flex; justify-content: center;",
                valueBox(
                  subtitle = "Total Earned TPE",
                  value = 
                    textOutput(ns("totalTPE"), inline = TRUE) %>% 
                    withSpinner(proxy.height = "20px"),
                  width = 3
                ),
                valueBox(
                  subtitle = "Available TPE",
                  value = 
                    textOutput(ns("remainingTPE"), inline = TRUE) %>% 
                    withSpinner(proxy.height = "20px"), 
                  width = 3
                )
              ) 
            ),
            fluidRow(
              column(
                width = 12,
                align = "center", 
                style = "display: flex; justify-content: center;",
                uiOutput(ns("activityCheck")),
                uiOutput(ns("trainingCamp"))
              )
            )
          )
        }
      ),
      ## Update Attributes
      fluidRow(
        box(
          title = "Player Attributes",
          collapsible = TRUE,
          width = NULL,
          # RENDER PLOT WITH ATTRIBUTES,
          fluidRow(
            column(
              width = 12,
              align = "center", 
              style = "display: flex; justify-content: center;",
              actionButton(
                inputId = ns("goToRegression"),
                "Regress"
              ),
              actionButton(
                inputId = ns("goToUpdate"),
                "Update"
              )
            )
          )
        ) %>% 
          div(id = "attributeBox"),
        box(
          title = "Player Attributes",
          collapsible = TRUE,
          width = NULL,
          fluidRow(
            column(
              width = 4,
              uiOutput(
                ns("physical")
              )
            ),
            column(
              width = 4,
              uiOutput(
                ns("mental")
              )
            ),
            column(
              width = 4,
              uiOutput(
                ns("specific")
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
                "Reset Build"
              ),
              actionButton(
                inputId = ns("verifyUpdate"),
                "Update"
              )
            )
          )
        ) %>% 
          div(id = "updateBox")
      ),
      ## History Information
      fluidRow(
        {
          box(
            title = "Updating History",
            collapsible = TRUE,
            width = NULL,
            fluidRow(
              column(
                width = 12,
                reactableOutput(ns("updateHistory"))
              )
            )
          )
        }
      ),
      fluidRow(
        {
          box(
            title = "TPE History",
            collapsible = TRUE,
            width = NULL,
            fluidRow(
              column(
                width = 12,
                reactableOutput(ns("tpeHistory"))
              )
            )
          )
        }
      )
    )
  )
}

playerPageServer <- function(id, userinfo) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ## Asynchronous reading of the player data
      playerDataAsync <- 
        reactive({
          getPlayerDataAsync(uid = userinfo$uid)
        }) 
      
      playerData <- reactiveValues(
        data = getPlayerData(uid = userinfo$uid)
      )
      
      tpeHistory <- reactiveValues(
        data = getTpeHistory(playerData$data$pid)
      )
      
      ## Sets the total TPE and the cost of the current build
      currentAvailable <- 
        reactiveVal({
          playerDataAsync() %...>%
            select(tpe)
        })
      
      currentCost <- 
        reactiveVal({
          playerDataAsync() %...>%
            select(acceleration:throwing) %...>%
            select(!`natural fitness` & !stamina) %...>% 
            pivot_longer(
              cols = everything(),
              names_to = "attribute",
              values_to = "value"
            ) %...>%
            left_join(
              tpeCost %>% 
                select(
                  value,
                  cumCost
                ),
              by = "value"
            ) %...>% 
            select(cumCost) %...>% 
            sum(na.rm = TRUE)
        })
      
      currentRemaining <- 
        reactive({
          currentAvailable() %...>%
            sum(-currentCost())
        })
      
      ## Observer for the change in total cost 
      observe({
        currentCost( 
          attributes %>% 
            filter(
              !attribute %in% c("Natural Fitness", "Stamina")
            ) %>% 
            select(attribute) %>% 
            unlist() %>% 
            stringr::str_to_title() %>% 
            lapply(
              X = .,
              FUN = function(x){
                tpeCost[tpeCost$value == session$input[[x]], "cumCost"]
              }
            ) %>% 
            unlist() %>% 
            sum()
        )
      })
      
      ## TPE texts
      output$totalTPE <- renderText({
        currentAvailable() %...>% 
          unlist() %...>% 
          unname()
      })
      
      output$remainingTPE <- renderText({
        currentRemaining()
      })
      
      ## Gets date of the start of the week in Pacific
      weekStart <- 
        lubridate::now() %>% 
        with_tz("US/Pacific") %>% 
        floor_date("week", week_start = "Monday")
      
      ## Updating attributes
      output$physical <- renderUI({
        tagList(
          splitLayout(
            "Attribute" %>% strong(),
            "Value" %>% strong(),
            "Cost" %>% strong(),
            cellWidths = c("50%", "25%", "25%"),
            cellArgs = list(style = "padding: 5px")
          ),
          c(
            "acceleration", "agility", "balance", "jumping reach", 
            "natural fitness", "pace", "stamina", "strength"
          ) %>% 
            stringr::str_to_title() %>% 
            map(
              .x = .,
              .f = 
                ~ 
                attributeEdit(
                  name = .x, 
                  value = 5, 
                  session = session,
                  update = TRUE
                )
            )
        )
      })
      
      output$mental <- renderUI({
        tagList(
          splitLayout(
            "Attribute" %>% strong(),
            "Value" %>% strong(),
            "Cost" %>% strong(),
            cellWidths = c("50%", "25%", "25%"),
            cellArgs = list(style = "padding: 5px")
          ),
          c(
            "aggression", "anticipation", "bravery", "composure", "concentration", 
            "decisions", "determination", "flair", "leadership", "off the ball", 
            "positioning", "teamwork", "vision", "work rate"
          ) %>% 
            map(
              .x = .,
              .f = 
                ~ 
                attributeEdit(
                  name = .x %>% stringr::str_to_title(), 
                  value = 5, 
                  session = session,
                  update = TRUE
                )
            )
        )
      })
      
      output$specific <- renderUI({
        tagList(
          splitLayout(
            "Attribute" %>% strong(),
            "Value" %>% strong(),
            "Cost" %>% strong(),
            cellWidths = c("50%", "25%", "25%"),
            cellArgs = list(style = "padding: 5px")
          ),
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
                ~ 
                attributeEdit(
                  name = .x, 
                  value = 5, 
                  session = session,
                  update = TRUE
                )
            )
        )
      })
      
      observe({
        input$goToUpdate
        
        attributes$attribute %>% 
        map(
          .x = .,
          .f = function(x){
            updateNumericInput(
              session = session,
              inputId = x,
              value = playerDataAsync() %...>% select(all_of(x)),
              min = playerDataAsync() %...>% select(all_of(x))
            )
          } 
        )
        
        # updateNumericInput(session = session, inputId = "Acceleration", value = 10, min = 10)  
      })
      
      
      # Create all the cost UIs
      attributes %>% 
        filter(
          !attribute %in% c("Natural Fitness", "Stamina")
        ) %>% 
        select(attribute) %>% 
        unlist() %>%  
        stringr::str_to_title() %>% 
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
      
      ## Observer for the verify update button that summarizes the updates and allows for it to be sent to the database
      
      # TODO: ADD VALIDATION https://rstudio.github.io/shinyvalidate/
      observeEvent(
        input$verifyUpdate,
        {
          if(currentAvailable() - currentCost() < 0){
            showModal(
              modalDialog(
                span(
                  "You have spent too much TPE on your attributes!"
                ),
                title="Too much TPE spent!",
                footer = 
                  tagList(
                    modalButton("Ok")
                  ),
                easyClose = TRUE
              )
            )
          } else {
            update <- 
              data.frame(
                attribute = 
                  playerData$data %>% 
                  select(acceleration:throwing) %>%
                  select(
                    where(
                      ~ !is.na(.x)
                    )
                  ) %>% 
                  colnames() %>% 
                  str_to_title(),
                new =
                  playerData$data %>%
                  select(acceleration:throwing) %>%
                  colnames() %>%
                  str_to_title() %>%
                  sapply(
                    X = .,
                    FUN = function(x) input[[x]],
                    simplify = TRUE
                  ) %>% 
                  unlist(),
                old = 
                  playerData$data %>% 
                  select(acceleration:throwing) %>% 
                  select(
                    where(
                      ~ !is.na(.x)
                    )
                  ) %>% 
                  t()
              ) %>% 
              filter(
                old != new
              )
            
            if(update %>% nrow() > 0){
              if(any((update$old - update$new) > 0)){
                showModal(
                  modalDialog(
                    span(
                      "You cannot reduce attributes in a regular update.",
                      paste("Return ", paste0(update$attribute[(update$old - update$new) > 0], collapse = ", "), " to their original values.")
                    ),
                    title="Reducing attributes!",
                    footer = 
                      tagList(
                        modalButton("Ok")
                      ),
                    easyClose = TRUE
                  )
                )
              } else {
                showModal(
                  modalDialog(
                    span(
                      "Are you sure you want to update these attributes?" %>% strong(),
                      style = "color: red;"
                    ),
                    br(),
                    column(
                      width = 8,
                      offset = 2,
                      helpText(
                        paste(
                          paste(
                            update$attribute,
                            paste(
                              update$old,
                              update$new,
                              sep = " -> "
                            )
                          ),
                          collapse = "<br>"
                        ) %>% 
                          HTML()
                      ) %>% 
                        div(
                          style = "background: #f0f0f0; border: #656565"
                        )
                    ),
                    br(),
                    tagList(
                      modalButton("No, go back"),
                      actionButton(
                        inputId = session$ns("confirmUpdate"),
                        label = "Yes, confirm update!"
                      )
                    ),
                    title="Update output",
                    footer = NULL,
                    easyClose = FALSE
                  )
                )
              }
            } else {
              showModal(
                modalDialog(
                  span(
                    "You have not changed your build yet, there is nothing to update."
                  ),
                  title="No changes made!",
                  footer = 
                    tagList(
                      modalButton("Ok")
                    ),
                  easyClose = TRUE
                )
              )
            }
          }
          
        },
        # This makes it so that the event doesn't trigger after a change in the data
        ignoreInit = TRUE
      )
      
      observeEvent(
        input$confirmUpdate,
        {
          removeModal()
          
          update <- 
            data.frame(
              attribute = 
                playerData$data%>% 
                select(acceleration:throwing) %>%
                select(
                  where(
                    ~ !is.na(.x)
                  )
                ) %>% 
                colnames() %>% 
                str_to_title(),
              new =
                playerData$data%>%
                select(acceleration:throwing) %>%
                colnames() %>%
                str_to_title() %>%
                sapply(
                  X = .,
                  FUN = function(x) input[[x]],
                  simplify = TRUE
                ) %>% 
                unlist(),
              old = 
                playerData$data%>% 
                select(acceleration:throwing) %>% 
                select(
                  where(
                    ~ !is.na(.x)
                  )
                ) %>% 
                t()
            ) %>% 
            filter(
              old != new
            )
          
          updateLog(uid = userinfo$uid, pid = playerData$data$pid, updates = update)
          
          updateBuild(pid = playerData$data$pid, updates = update)
          
          playerData$data <- getPlayerData(pid = playerData$data$pid)
        },
        # This makes it so that the event doesn't trigger twice
        ignoreInit = TRUE
      )
      
      observeEvent(
        input$resetUpdate,
        {
          ## Updating attributes
          output$physical <- renderUI({
            tagList(
              splitLayout(
                "Attribute" %>% strong(),
                "Value" %>% strong(),
                "Cost" %>% strong(),
                cellWidths = c("50%", "25%", "25%"),
                cellArgs = list(style = "padding: 5px")
              ),
              colnames(playerData$data%>% select(acceleration:strength)) %>% 
                map(
                  .x = .,
                  .f = 
                    ~ 
                    attributeEdit(
                      name = .x %>% stringr::str_to_title(), 
                      value = playerData$data[,.x], 
                      session = session
                    )
                )
            )
          })
          
          output$mental <- renderUI({
            tagList(
              splitLayout(
                "Attribute" %>% strong(),
                "Value" %>% strong(),
                "Cost" %>% strong(),
                cellWidths = c("50%", "25%", "25%"),
                cellArgs = list(style = "padding: 5px")
              ),
              colnames(playerData$data%>% select(aggression:`work rate`)) %>% 
                map(
                  .x = .,
                  .f = 
                    ~ 
                    attributeEdit(
                      name = .x %>% stringr::str_to_title(), 
                      value = playerData$data[,.x], 
                      session = session
                    )
                )
            )
          })
          
          output$specific <- renderUI({
            tagList(
              splitLayout(
                "Attribute" %>% strong(),
                "Value" %>% strong(),
                "Cost" %>% strong(),
                cellWidths = c("50%", "25%", "25%"),
                cellArgs = list(style = "padding: 5px")
              ),
              colnames(
                playerData$data%>% 
                  select(corners:technique, `aerial reach`:throwing) %>% 
                  select(
                    where(function(x) !is.na(x))
                  )) %>% 
                map(
                  .x = .,
                  .f = 
                    ~ 
                    attributeEdit(
                      name = .x %>% stringr::str_to_title(), 
                      value = playerData$data[,.x], 
                      session = session
                    )
                )
            )
          })
        },
        # This makes it so that the event doesn't trigger twice
        ignoreInit = TRUE
      )
      
      
      ## Gets update history
      output$updateHistory <- renderReactable({
        
        getUpdateHistory(playerData$data$pid) %>% 
          reactable()
        
      })
      
      ## Gets TPE history
      output$tpeHistory <- renderReactable({
        
        getTpeHistory(playerData$data$pid) %>%
          reactable()
        
      })
      
      ## UI Outputs for TPE
      output$activityCheck <- 
        renderUI({
          completed <- 
            (
              tpeHistory$data %>% 
              filter(time > (weekStart), source == "Activity Check") %>% 
              nrow()
            ) > 0
          
          if(completed){
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
      
      output$trainingCamp <- 
        renderUI({
          completed <- 
            (
              tpeHistory$data %>% 
                filter(
                  time > 
                    (currentSeason$startDate %>% 
                       as_date() %>% 
                       force_tz("US/Pacific")
                     ), 
                  source %>% str_detect(pattern = "Training Camp")
                ) %>% 
                nrow()
            ) > 0
          
          if(completed){
            # Show no button at all
          } else {
            actionButton(
              session$ns("trainingCamp"),
              "Seasonal Training Camp"
            )
          }
          
        })
      
      observeEvent(
        input$trainingCamp,
        {
          
          age <- 
            currentSeason$season - 
            (playerData$data$class %>% 
               str_extract_all(pattern = "[0-9]+") %>% 
               as.numeric()
             )
          
          tpe <- 
            data.frame(
              source = paste("S", currentSeason$season, " Training Camp", sep = ""),
              tpe = case_when(
                age <= 2 ~ 40,
                age <= 5 ~ 30,
                age <= 8 ~ 20,
                TRUE ~ 10
              )
            )
          
          tpeLog(uid = userinfo$uid, pid = playerData$data$pid, tpe = tpe)
          
          shinyjs::disable(session$ns("trainingCamp"))
          
          updateTPE(pid = playerData$data$pid, tpe = tpe)
          
          playerData$data <- getPlayerData(pid = playerData$data$pid)
          
          tpeHistory$data <- getTpeHistory(pid = playerData$data$pid)
        },
        # This makes it so that the event doesn't trigger after a change in the data
        ignoreInit = TRUE
      )
      
      observeEvent(
        input$activityCheck,
        {
          tpe <- 
            data.frame(
              source = "Activity Check",
              tpe = 6
            )
          
          tpeLog(uid = userinfo$uid, pid = playerData$data$pid, tpe = tpe)
          
          shinyjs::disable(session$ns("activityCheck"))
          
          updateTPE(pid = playerData$data$pid, tpe = tpe)
          
          playerData$data <- getPlayerData(pid = playerData$data$pid)
          
          tpeHistory$data <- getTpeHistory(pid = playerData$data$pid)
        },
        # This makes it so that the event doesn't trigger after a change in the data
        ignoreInit = TRUE
      )
      
      
      output$user <- renderPrint({ 
        list(
          class(currentAvailable()),
          class(currentCost()),
          class(currentRemaining())
        )
      })
    }
  )
}