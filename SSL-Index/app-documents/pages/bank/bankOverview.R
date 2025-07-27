bankOverviewUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        playerInfoBoxUI(id = ns("playerInfo")) %>% 
          withSpinnerSmall()
      )
    ),
    fluidRow(
      column(
        width = 12,
        h5("Bank Transactions"),
        reactableOutput(ns("historyBank")) %>% withSpinnerMedium()
      )
    ),
    br(),
    br(),
    fluidRow(
      column(width = 12,
             uiOutput(ns("purchaseTraining")) %>% 
               withSpinnerMedium()
             )
      ),
    fluidRow(
      column(width = 12,
             uiOutput(ns("purchaseTraits")) %>% 
               withSpinnerMedium())
      ),
    fluidRow(
      column(width = 12,
             uiOutput(ns("purchasePositions")) %>% 
               withSpinnerMedium())
    ),
    fluidRow(
      column(width = 12,
             uiOutput(ns("purchaseFootedness")) %>% 
               withSpinnerMedium())
    ),
    fluidRow(
      column(width = 6, offset = 3,
             div(
               class = "frozen-bottom",
               align = "center",
                 fluidRow(
                   uiOutput(ns("totalSum")) %>% 
                     withSpinnerSmall()
                 ),
                 fluidRow(
                   actionButton(ns("verifyPurchase"),
                                label = "Verify purchase")
                 )
             )
      )
    ),
    div(style = "min-height:100px;")
  )
}

bankOverviewServer <- function(id, uid, parent, updated) {
  moduleServer(
    id,
    function(input, output, session) {
      purchased <- reactiveVal({0})
      
      
      player <- reactive({
        getPlayerName(userID = uid)
      }) %>% 
        bindEvent(
          purchased(),
          updated(),
          ignoreInit = FALSE,
          ignoreNULL = FALSE
        )
      
      traitSum <- reactiveVal({0})
      positionSum <- reactiveVal({0})
      footSum <- reactiveVal({0})
      
      observe({
        player() %>% 
          then(
            onFulfilled = function(data){
              playerInfoBoxServer(id = "playerInfo", pid = data$pid, mainSession = parent)
            }
          )
      })
      
      historyBank <- 
        reactive({
          player() %>% 
            then(
              onFulfilled = function(value){
                future_promise({
                  readAPI("https://api.simulationsoccer.com/bank/getBankTransactions",
                          query = list(pid = value$pid)
                  ) %>%
                    select(!pid)
                })
                # getBankTransactions(value$pid)
              }
            )
        })
      
      output$historyBank <- renderReactable({
        historyBank() %>% 
          then(
            onFulfilled = function(value){
              value %>% 
                mutate(
                  Time = as_datetime(Time)
                ) %>% 
                reactable(
                  columns = 
                    list(
                      Time = colDef(format = colFormat(datetime = TRUE)),
                      Transaction = colDef(format = colFormat(digits = 0, separators = TRUE, currency = "USD"))
                    )
                )
            },
            onRejected = NULL
          )
      })
      
      output$totalSum <- renderUI({
        h4(paste("Total sum of purchase:", 
                 sum(
                   c(input$individualTraining * 425000, 
                     traitSum(),
                     positionSum(),
                     footSum()
                     ) %>% as.numeric()) %>% dollar()))
      })
      
      #### INDIVIDUAL TRAINING ####
      output$purchaseTraining <- renderUI({
        player() |> 
          then(
            onFulfilled = function(data){
              tagList(
                p(
                  "You may purchase at most 18 TPE from individual training per season. Each TPE purchased
            cost $425 000 for a total cost of $7.65 million for the entire 18 TPE."
                ),
                sliderInput(session$ns("individualTraining"),
                            label = "Select the number of TPE:",
                            min = 0,
                            max = 18 - data$purchasedTPE,
                            value = 0,
                            step = 1,
                            ticks = FALSE,
                            width = "80%"
                )
              ) %>% 
                box(title = "Individual Training", collapsible = TRUE, width = NULL, collapsed = TRUE)
            }
          ) 
      })
      
      #### TRAITS ####
      output$purchaseTraits <- renderUI({
        player() %>% 
          then(
            onFulfilled = function(data){
              getPlayerTraits(playerID = data$pid) %>% 
                then(
                  onFulfilled = function(currentTraits){
                    nrTraits <- length(currentTraits)
                    tagList(
                      p(
                        paste(
                          "Each new trait costs $3 million and your player can have a maximum of 7 traits, including the 2 you started with.",
                          "The traits are separated into seven different categories depending on what part of the play they affect. Note that 
              some of the traits will negatively impact your player if their attributes does not match. If you notice a disabled 
              trait in the list, it is not compatible with one or more of your current traits. The full list of incompatible traits 
              can be found in the", tags$a("Player Handbook.", href = "https://docs.google.com/document/d/1cp4OdU43nX8A7kbQVmOl89xZRD3l13voHcqLNrxFL4Q/edit#", target = "_blank"),
                          "If you want to remove an already selected player trait, you can purchase this item ($0.5 million)."
                        ) %>% HTML()
                      ),
                      checkboxGroupInput(
                        session$ns("traits"), 
                        paste("Change your traits"), 
                        choices = traits %>% unlist(use.names = FALSE), 
                        selected = currentTraits
                      ) %>% 
                        div(class = "multicol"),
                      tags$script(paste("
                          Shiny.addCustomMessageHandler('disableCheckbox', function(checkboxId) {
                            if (typeof checkboxId === 'string') {
                              checkboxId = [checkboxId]; // Convert single string to array
                            }
                            var checkboxes = document.getElementsByName('", session$ns('traits'), "');
                            for (var i = 0; i < checkboxes.length; i++) {
                              checkboxes[i].disabled = false; // Disable specific checkboxes
                            }
                            for (var i = 0; i < checkboxes.length; i++) {
                              for (var j = 0; j < checkboxId.length; j++) {
                                if(checkboxes[i].value == checkboxId[j]){
                                  checkboxes[i].disabled = true; // Disable specific checkboxes
                                } else {
                                  
                                }
                              }
                            }
                          });
                        ", sep = "") %>% HTML()
                      )
                    ) %>% 
                      box(title = "Player Traits", collapsible = TRUE, width = NULL, collapsed = TRUE)
                  }
                )
            }
          )
        
      })
      
      # Disables clashing traits
      observe({
        selected <- input$traits
        
        disable_list <- character()
        if ("Cuts Inside From Both Wings" %in% selected) {
          disable_list <- c(disable_list, "Avoids Using Weaker Foot")
        }
        if ("Knocks Ball Past Opponent" %in% selected) {
          disable_list <- c(disable_list, "Runs With Ball Rarely")
        }
        if ("Runs With Ball Rarely" %in% selected) {
          disable_list <- c(disable_list, "Knocks Ball Past Opponent", "Runs With Ball Often", "Runs With Ball Down Left", "Runs With Ball Down Right", "Runs With Ball Through Centre")
        }
        if ("Runs With Ball Often" %in% selected) {
          disable_list <- c(disable_list, "Runs With Ball Rarely")
        }
        if ("Runs With Ball Down Left" %in% selected) {
          disable_list <- c(disable_list, "Runs With Ball Down Right", "Runs With Ball Through Centre", "Runs With Ball Rarely")
        }
        if ("Runs With Ball Down Right" %in% selected) {
          disable_list <- c(disable_list, "Runs With Ball Down Left", "Runs With Ball Through Centre", "Runs With Ball Rarely")
        }
        if ("Runs With Ball Through Centre" %in% selected) {
          disable_list <- c(disable_list, "Runs With Ball Down Left", "Runs With Ball Down Right", "Runs With Ball Rarely")
        }
        if ("Arrives Late In Opponent's Area" %in% selected) {
          disable_list <- c(disable_list, "Stays Back At All Times", "Gets Into Opposition Area")
        }
        if ("Gets Into Opposition Area" %in% selected) {
          disable_list <- c(disable_list, "Arrives Late In Opponent's Area", "Hugs Line", "Stays Back At All Times")
        }
        if ("Comes Deep To Get Ball" %in% selected) {
          disable_list <- c(disable_list, "Gets Forward Whenever Possible", "Likes To Try To Beat Offside Trap")
        }
        if ("Gets Forward Whenever Possible" %in% selected) {
          disable_list <- c(disable_list, "Comes Deep To Get Ball", "Stays Back At All Times", "Hugs Line")
        }
        if ("Likes To Try To Beat Offside Trap" %in% selected) {
          disable_list <- c(disable_list, "Comes Deep To Get Ball", "Does Not Move Into Channels", "Plays With Back To Goal")
        }
        if ("Hugs Line" %in% selected) {
          disable_list <- c(disable_list, "Gets Into Opposition Area")
        }
        if ("Plays With Back To Goal" %in% selected) {
          disable_list <- c(disable_list, "Likes To Try To Beat Offside Trap")
        }
        if ("Does Not Move Into Channels" %in% selected) {
          disable_list <- c(disable_list, "Moves Into Channels", "Likes To Try To Beat Offside Trap")
        }
        if ("Moves Into Channels" %in% selected) {
          disable_list <- c(disable_list, "Does Not Move Into Channels", "Stays Back At All Times")
        }
        if ("Stays Back At All Times" %in% selected) {
          disable_list <- c(disable_list, "Arrives Late In Opponent's Area", "Gets Forward Whenever Possible", "Gets Into Opposition Area", "Moves Into Channels")
        }
        if ("Likes To Switch Ball To Other Flank" %in% selected) {
          disable_list <- c(disable_list, "Plays Short Simple Passes")
        }
        if ("Looks For Pass Rather Than Attempting To Score" %in% selected) {
          disable_list <- c(disable_list, "Tries First Time Shots")
        }
        if ("Plays No Through Balls" %in% selected) {
          disable_list <- c(disable_list, "Tries Killer Balls Often")
        }
        if ("Plays Short Simple Passes" %in% selected) {
          disable_list <- c(disable_list, "Likes To Switch Ball To Other Flank", "Tries Killer Balls Often", "Tries Long Range Passes")
        }
        if ("Tries Killer Balls Often" %in% selected) {
          disable_list <- c(disable_list, "Plays Short Simple Passes", "Plays No Through Balls")
        }
        if ("Tries Long Range Passes" %in% selected) {
          disable_list <- c(disable_list, "Plays Short Simple Passes")
        }
        if ("Hits Free Kicks With Power" %in% selected) {
          disable_list <- c(disable_list, "Refrains From Taking Long Shots")
        }
        if ("Places Shots" %in% selected) {
          disable_list <- c(disable_list, "Shoots With Power")
        }
        if ("Refrains From Taking Long Shots" %in% selected) {
          disable_list <- c(disable_list, "Hits Free Kicks With Power", "Tries Long Range Free Kicks")
        }
        if ("Shoots From Distance" %in% selected) {
          disable_list <- c(disable_list, "Looks For Pass Rather Than Attempting To Score", "Refrains From Taking Long Shots")
        }
        if ("Shoots With Power" %in% selected) {
          disable_list <- c(disable_list, "Places Shots")
        }
        if ("Tries First Time Shots" %in% selected) {
          disable_list <- c(disable_list, "Looks For Pass Rather Than Attempting To Score")
        }
        if ("Tries Long Range Free Kicks" %in% selected) {
          disable_list <- c(disable_list, "Refrains From Taking Long Shots")
        }
        if ("Shoots From Distance" %in% selected) {
          disable_list <- c(disable_list, "Refrains From Taking Long Shots")
        }
        if ("Dives Into Tackles" %in% selected) {
          disable_list <- c(disable_list, "Does Not Dive Into Tackles")
        }
        if ("Does Not Dive Into Tackles" %in% selected) {
          disable_list <- c(disable_list, "Dives Into Tackles")
        }
        if ("Avoids Using Weaker Foot" %in% selected) {
          disable_list <- c(disable_list, "Cuts Inside From Both Wings")
        }
        if ("Tries To Play Way Out Of Trouble" %in% selected) {
          disable_list <- c(disable_list, "Runs With Ball Rarely")
        }
        
        # if(length(disable_list) == 0){
        #   disable_list <- "none"
        # }
        
        session$sendCustomMessage("disableCheckbox", disable_list %>% unique())
      }) %>% 
        bindEvent(
          input$traits,
          ignoreInit = FALSE,
          ignoreNULL = FALSE
        )
      
      # Calculates sum of purchase
      shiny::observe({
        player() %>% 
          then(
            onFulfilled = function(data){
              getPlayerTraits(playerID = data$pid) %>% 
                then(
                  onFulfilled = function(currentTraits){
                    nrTraits <- length(currentTraits)
                    
                    removed <- sum(!(currentTraits %in% input$traits))
                    added <- sum(!(input$traits %in% currentTraits))
                    
                    traitSum(removed * 500000 + added * 3000000)
                  }
                )
            }
          )
      }) %>% 
        bindEvent(
          input$traits,
          ignoreInit = TRUE,
          ignoreNULL = FALSE
        )
      
      #### POSITION XP ####
      output$purchasePositions <- renderUI({
        player() %>% 
          then(
            onFulfilled = function(data){
              getPlayerPositions(playerID = data$pid) %>% 
                then(
                  onFulfilled = function(currentPositions){
                    if(currentPositions %>% filter(name == "GK") %>% .$value != 20){
                      posPrim <- positions[names(positions) %in% (currentPositions %>% 
                                                                    filter(value == 20) %>% 
                                                                    select(name) %>% unlist())
                      ]
                      
                      posSec <- positions[names(positions) %in% (currentPositions %>% 
                                                                   filter(value < 20 & value > 5) %>% 
                                                                   select(name) %>% unlist())
                      ]
                      
                      posRem <- positions[!(positions %in% c(posPrim, posSec))]
                      
                      tagList(
                        p(paste("At creation, a player starts with one primary and two secondary positions. With purchases in the Player Store you are able to switch, upgrade or add positions to your player.",
                                "The items available are:",
                                tags$ul(
                                  tags$li("Position Transfer ($5 million): A transfer consists of swapping two positions between any boxes."),
                                  tags$li("Position Upgrade ($7.5 million): An upgrade consists of moving a position from the secondary to primary box."),
                                  tags$li("Position Addition ($12.5 million): An addition consists of moving a 'new' position to the secondary position box. If you move a position to the primary box this is a Addition + Upgrade.")
                                )
                        ) %>% HTML()),
                        bucket_list(
                          header = NULL,
                          group_name = session$ns("pos"),
                          orientation = "horizontal",
                          add_rank_list(
                            text = withTooltip("PRIMARY POSITION(S)", "Converts to 20 positional experience in the position"),
                            labels = posPrim,
                            input_id = session$ns("primary")
                          ),
                          add_rank_list(
                            text = withTooltip("SECONDARY POSITION(S)", "Converts to 15 positional experience in the position"),
                            labels = posSec,
                            input_id = session$ns("secondary")
                          ),
                          add_rank_list(
                            text = "Drag from here",
                            labels = posRem,
                            input_id = session$ns("unusedPositions")
                          )
                        )
                      ) %>% 
                        box(title = "Player Positions", collapsible = TRUE, width = NULL, collapsed = TRUE)
                    }
                  }
                )
            }
          )
      })
      
      observe({
        player() %>% 
          then(
            onFulfilled = function(data){
              getPlayerPositions(playerID = data$pid) %>% 
                then(
                  onFulfilled = function(currentPositions){
                    summary <- 
                      tibble(
                        name = c(input$primary, input$secondary, input$unusedPositions),
                        value = c(rep(20, times = length(input$primary)), rep(15, times = length(input$secondary)), rep(0, times = length(input$unusedPositions)))
                      ) %>% 
                      left_join(
                        currentPositions,
                        by = "name",
                        suffix = c(".new", ".old")
                      ) %>% 
                      mutate(
                        change = case_when(
                          value.old == 0 & value.new == 20 ~ "Addition + Upgrade",
                          value.old == 0 & value.new == 15 ~ "Addition",
                          value.old >= 5 & value.old <  20 & value.new == 20 ~ "Upgrade",
                          value.old == 20& value.new == 15 ~ "Downgrade",
                          value.old == 20& value.new == 0  ~ "Downgrade + Remove",
                          value.old >= 5 & value.old <  20 & value.new == 0  ~ "Remove",
                          TRUE ~ "No Change"
                        )
                      ) %>% 
                      group_by(change) %>% 
                      summarize(n = n()) %>% 
                      mutate(
                        cost = case_when(
                          change == "Addition + Upgrade" ~ n*20000000,
                          change == "Upgrade" ~ n*7500000,
                          change == "Addition" ~ n*12500000,
                          change == "Downgrade + Remove" ~ -n*20000000,
                          change == "Downgrade" ~ -n*7500000,
                          change == "Remove" ~ -n*12500000,
                          TRUE ~ 0
                        )
                      ) %>% 
                      ungroup()
                    
                    # print(summary)
                    
                    ## Calculates number of swaps
                    swaps <- 
                      summary %>%
                      summarize(
                        Addition = if_else(str_detect(change, "^Addition$"), n, 0),
                        Remove = if_else(str_detect(change, "^Remove$"), n, 0),
                        Upgrade = if_else(str_detect(change, "^Upgrade$"), n, 0),
                        Downgrade = if_else(str_detect(change, "^Downgrade$"), n, 0),
                        AdditionPlus = if_else(str_detect(change, "Addition +"), n, 0),
                        RemovePlus = if_else(str_detect(change, "Downgrade +"), n, 0)
                      ) %>% 
                      summarise(
                        Addition = sum(Addition),
                        Remove = sum(Remove),
                        Upgrade = sum(Upgrade),
                        Downgrade = sum(Downgrade),
                        AdditionPlus = sum(AdditionPlus),
                        RemovePlus = sum(RemovePlus)
                      ) %>% 
                      summarize(
                        swaps = floor((Addition + Remove)/2) + floor((Upgrade + Downgrade)/2) + floor((AdditionPlus + RemovePlus)/2)
                      )
                    
                    # print(swaps)
                    
                    summary <-
                      summary %>% 
                      mutate(
                        cost = if_else(change == "No Change", cost + if_else(swaps$swaps > 0, swaps$swaps, 0) * 5000000, cost)
                      )
                    
                    
                    # print(summary)
                    
                    ## Only calculate cost if all 13 positions are accounted for
                    if(summary$n %>% sum() == 13){
                      positionSum({sum(summary$cost)})
                    }
                    
                    
                    
                    if(summary$cost %>% sum() < 0){
                      showToast(.options = myToastOptions,"error", "You cannot only remove positions from your build, please adjust your positional purchase.")
                      
                      updateActionButton(inputId = "verifyPurchase", label = "You cannot only remove positions!")
                      disable(id = "verifyPurchase")
                    } else {
                      updateActionButton(inputId = "verifyPurchase", label = "Verify purchase")
                      enable(id = "verifyPurchase")
                    }
                    
                  }
                )
            }
          )
      }) %>% 
        bindEvent(
          input$pos,
          ignoreInit = TRUE
        )
      
      #### FOOTEDNESS ####
      output$purchaseFootedness <- renderUI({
        player() %>% 
          then(
            onFulfilled = function(data){
              getPlayerFootedness(playerID = data$pid) %>% 
                then(
                  onFulfilled = function(currentFootedness){
                    tagList(
                      p("The footedness of a player is split into a main (20) and weak (10) foot based on the preferred footedness of the player 
                        selected at creation. You can increase your weak foot proficiency by increments of 5 for $7.5 million."),
                      column(
                        width = 6,
                        selectInput(session$ns("left"), label = "Left Foot", choices = seq(currentFootedness$`left foot`, 20, by = 5), selected = currentFootedness$`left foot`)  
                      ),
                      column(
                        width = 6,
                        selectInput(session$ns("right"), label = "Right Foot", choices = seq(currentFootedness$`right foot`, 20, by = 5), selected = currentFootedness$`right foot`) 
                      )
                    ) %>% 
                      box(title = "Player Footedness", collapsible = TRUE, width = NULL, collapsed = TRUE)
                  }
                )
            }
          )
      })
      
      shiny::observe({
        player() %>%
          then(
            onFulfilled = function(data){
              getPlayerFootedness(playerID = data$pid) %>%
                then(
                  onFulfilled = function(currentFootedness){
                    if(input$left %>% is.na() | input$right %>% is.na()){
                      # DO NOTHING
                    } else {
                      
                      left <- input$left %>% as.numeric()
                      right <- input$right %>% as.numeric()
                      
                      # if(input$left > 20){
                      #   updateNumericInput(inputId = "left", value = 20)
                      # }
                      # if(input$right > 20){
                      #   updateNumericInput(inputId = "right", value = 20)
                      # }
                      # if(input$left < currentFootedness$`left foot`){
                      #   updateNumericInput(inputId = "left", value = currentFootedness$`left foot`)
                      # }
                      # if(input$right < currentFootedness$`right foot`){
                      #   updateNumericInput(inputId = "right", value = currentFootedness$`right foot`)
                      # }
                      # 
                      ## Only change the cost if the footedness is within the allowed constraints
                      if(left >= currentFootedness$`left foot` & left <= 20 & right >= currentFootedness$`right foot` & right <= 20){
                        footSum(((left - currentFootedness$`left foot`) + (right - currentFootedness$`right foot`)) / 5 * 7500000)  
                      }
                    }
                  }
                )
            }
          )
      }) %>%
        bindEvent(
          input$left, input$right,
          ignoreInit = TRUE
        )
      
      #### VERIFY PURCHASE ####
      observe({
        shinyjs::disable(session$ns("verifyPurchase"))
        
        totalCost <- sum(
          c(input$individualTraining * 425000, 
            traitSum(),
            positionSum(),
            footSum()
          ) %>% as.numeric())
        
        if(totalCost < 0){
          showToast(.options = myToastOptions,"error", "You cannot make a negative purchase!")
        } else if(totalCost == 0){
          showToast(.options = myToastOptions,"warning", "You have not purchased anything yet.")
        } else {
          player() %>% 
            then(
              onFulfilled = function(data){
                readAPI(url = "https://api.simulationsoccer.com/bank/getBankBalance", query = list(pid = data$pid)) %>% 
                  future_promise() %>% 
                  then(
                    onFulfilled = function(balance){
                      if(totalCost > balance$balance){
                        showToast(.options = myToastOptions,"error", "Your purchase has exceeded your bank balance. Please revise your purchase.")
                      } else {
                        promise_all(
                          traits = getPlayerTraits(data$pid),
                          foot = getPlayerFootedness(data$pid),
                          positions = getPlayerPositions(data$pid)
                        ) %...>%
                          with({
                            update <- 
                              tibble(
                                attribute = c("traits"),
                                old = paste0(traits, collapse = traitSep),
                                new = paste0(input$traits, collapse = traitSep)
                              ) %>% 
                              add_row(
                                attribute = "left foot",
                                old = foot$`left foot` %>% as.character(),
                                new = input$left %>% as.character()
                              ) %>% 
                              add_row(
                                attribute = "right foot",
                                old = foot$`right foot` %>% as.character(),
                                new = input$right %>% as.character()
                              ) 
                            
                            if(c(input$primary, input$secondary, input$unusedPositions) %>% length() != 0){
                              update <- 
                                update %>% 
                                add_row(
                                  tibble(
                                    attribute = c(input$primary, input$secondary, input$unusedPositions),
                                    value = c(rep(20, times = length(input$primary)), rep(15, times = length(input$secondary)), rep(0, times = length(input$unusedPositions))) %>% as.character()
                                  ) %>% 
                                    left_join(
                                      positions,
                                      by = c("attribute" = "name"),
                                      suffix = c(".new", ".old")
                                    ) %>% 
                                    rename(
                                      old = value.old,
                                      new = value.new
                                    ) %>% 
                                    mutate(
                                      old = old %>% as.character()
                                    )
                                )
                            }
                            
                            update <- 
                              update %>% 
                              add_row(
                                tibble(
                                  attribute = "tpe",
                                  old = "0",
                                  new = input$individualTraining |> as.character()
                                )
                              )
                            
                            update <- 
                              update %>% 
                              filter(
                                new != old
                              )
                            
                            modalBankVerify(update, cost = totalCost, session = session)
                            
                          }) # close with
                      } # close else
                    } # close after bank balance
                  )
              } # close after player pid
            )          
        } # close else after purchase
        
        shinyjs::enable(session$ns("verifyPurchase"))
      }) %>% 
        bindEvent(
          input$verifyPurchase,
          ignoreInit = FALSE
        )
      
      #### CONFIRM PURCHASE ####
      
      observe({
        shinyjs::disable(session$ns("confirmPurchase"))
        
        totalCost <- sum(
          c(input$individualTraining * 425000, 
            traitSum(),
            positionSum(),
            footSum()
          ) %>% as.numeric())
        
        player() %>%
          then(
            onFulfilled = function(data){
              promise_all(
                traits = getPlayerTraits(data$pid),
                foot = getPlayerFootedness(data$pid),
                positions = getPlayerPositions(data$pid),
                purchases = readAPI("https://api.simulationsoccer.com/bank/getBankTransactions",
                                    query = list(pid = data$pid)) %>% select(!pid) %>% 
                  slice_head(n = 1) %>% 
                  future_promise()
              ) %...>%
                with({
                  removeModal()
                  
                  if(((now() %>% as.numeric()) - purchases$Time) < 60){
                    showToast(.options = myToastOptions,"error", "You have already made a purchase within the last minute. To prevent accidental double purchases, you need to wait until you make another purchase.")
                  } else {
                    update <- 
                      tibble(
                        attribute = c("traits"),
                        old = paste0(traits, collapse = traitSep),
                        new = paste0(input$traits, collapse = traitSep)
                      ) %>% 
                      add_row(
                        attribute = "left foot",
                        old = foot$`left foot` %>% as.character(),
                        new = input$left %>% as.character()
                      ) %>% 
                      add_row(
                        attribute = "right foot",
                        old = foot$`right foot` %>% as.character(),
                        new = input$right %>% as.character()
                      )
                    
                    
                    if(c(input$primary, input$secondary, input$unusedPositions) %>% length() != 0){
                      update <- 
                        update %>% 
                        add_row(
                          tibble(
                            attribute = paste("pos_", c(input$primary, input$secondary, input$unusedPositions) %>% str_to_lower(), sep = ""),
                            value = c(rep(20, times = length(input$primary)), rep(15, times = length(input$secondary)), rep(0, times = length(input$unusedPositions))) %>% as.character()
                          ) %>% 
                            left_join(
                              positions %>% 
                                mutate(
                                  name = paste("pos_", c(name) %>% str_to_lower(), sep = "")
                                ),
                              by = c("attribute" = "name"),
                              suffix = c(".new", ".old")
                            ) %>% 
                            rename(
                              old = value.old,
                              new = value.new
                            ) %>% 
                            mutate(
                              old = old %>% as.character()
                            )
                        ) 
                    }
                    
                    tpe <- tibble(
                      source = "Individual Training",
                      tpe = input$individualTraining
                    )
                    
                    update <- 
                      update %>% 
                      filter(
                        new != old
                      )
                    
                    if(nrow(update) > 0){
                      updateLog(uid, data$pid, updates = update)
                      updateBuild(pid = data$pid, updates = update, bank = NULL)
                    }
                    
                    if(tpe$tpe > 0){
                      tpeLog(uid, data$pid, tpe)
                      updateTPE(data$pid, tpe)
                      portalQuery(
                        query = "UPDATE playerdata SET purchasedTPE = purchasedTPE + ?tpe WHERE pid = ?pid;",
                        tpe = input$individualTraining,
                        pid = data$pid,
                        type = "set"
                      )
                    }
                    
                    addBankTransaction(uid = uid, pid = data$pid, source = "Store Purchase", transaction = -totalCost)
                    showToast(.options = myToastOptions,"success", "You have successfully completed your purchase!")
                    purchased(purchased() + 1)
                    updated(updated() + 1)
                  }
                })
            }
          )
        shinyjs::enable(session$ns("confirmPurchase"))
      }) %>% 
        bindEvent(
          input$confirmPurchase
        )
    }
  )
}
