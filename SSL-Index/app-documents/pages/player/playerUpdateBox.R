playerUpdateBoxUI <- function(id) {
  ns <- NS(id)
  tagList(
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
      hidden()
  )
}

playerUpdateBoxServer <- function(id, pid, uid, data, tpeTotal = tpeTotal, tpeBanked = tpeBanked, updating, updated = updated) {
  moduleServer(
    id,
    function(input, output, session) {
      #### OUTFIELD OUTPUTS ####
      # Player selector output
      output$selectPlayer <- renderUI({
        data() %>% 
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
      
      # Dynamic UI for position selector
      output$positionSelector <- renderUI({
        data() %>% 
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
                    text = withTooltip(paste("Select", length(posPrim), "primary position:"), "Converts to 20 positional experience in the position"),
                    labels = posPrim,
                    input_id = session$ns("primary")
                  ),
                  add_rank_list(
                    text = withTooltip(paste("Select", length(posSec), "secondary positions:"), "Converts to 15 positional experience in the position"),
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
        data() %>% 
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
                  ", sep = "") %>% HTML())
              )
            }
          )
      })
      
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
      
      #### COST OUTPUT ####
      # All the cost outputs
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
      
      # Dynamic technical attributes 
      output$technical <- renderUI({
        data() %>% 
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
                        ~ attributeInput(ns = session$ns, name = .x, value = if_else(updating() == "rerolling", 5, data[,.]))
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
                        ~ attributeInput(ns = session$ns, name = .x, value = if_else(updating() == "rerolling", 5, data[,.]))
                    )
                }
              }
            }
          )
      })
      
      # Starting the processing for updating, redistributing or regression
      output$verifyButton <- renderUI({
        actionButton(
          inputId = session$ns("verifyUpdate"),
          if_else(updating() == "updating", "Update", if_else(updating() == "redistributing", "Redistribute", "Reroll"))
        )
      })
      
      #### UPDATING ####
      # Set attribute UI to current build and 
      # fixes minimum value when updating
      # fixes maximum value when regressing
      # only resets value when resetting
      observe({
        data() %>% 
          then(
            onFulfilled = function(data){
              shinyjs::hide("attributeOverview")
              shinyjs::show("attributeUpdate")
              shinyjs::show("tpeButtons")
              shinyjs::show("buttonsUpdating")
              
              updating("updating")
              
              data %>% 
                select(acceleration:throwing) %>% 
                select(!c(`natural fitness`, stamina)) %>% 
                colnames() %>% 
                map(
                  .x = .,
                  .f = function(x){
                    updateNumericInput(
                      session = session,
                      inputId = x %>% stringr::str_to_title() %>% str_remove_all(pattern = " "),
                      value = data[, x],
                      min = data[, x],
                      max = 20
                    )
                  }
                )
              
              data %>% 
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
            }
          )
      }) %>% 
        bindEvent(
          input$goToUpdate,
          ignoreInit = TRUE
        )
      
      observe({
        data() %>% 
          then(
            onFulfilled = function(data){
              bank <- tpeBanked()
              
              if(bank < 0){
                showToast("error", "You have spent too much TPE on your attributes! Reduce some of your attributes and try again.")
              } else if(any(editableAttributes %>% sapply(X = ., FUN = function(att){input[[att]] > 20 | input[[att]] < 5}, simplify = TRUE) %>% unlist())){
                showToast("error", "One or more of your attributes are lower than 5 or higher than 20, which exceeds the allowed range of attribute values.")
              } else if(updating() == "updating"){
                ##### UPDATING #####
                update <- 
                  updateSummary(current = data, inputs = input) %>% 
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
                
                if(nrow(update) > 0){
                  if(any((update$old - update$new) > 0)){
                    showToast("error", paste("You cannot reduce attributes in a regular update.",
                                             paste("Return ", paste0(update$attribute[(update$old - update$new) > 0], collapse = ", "), " to their original values.")))
                  } else {
                    modalVerify(update, session = session)
                  }
                } else {
                  showToast(type = "warning", "You have not changed your build yet, there is nothing to update.")
                }
              } else {
                ##### REDISTRIBUTING and REROLLING #####
                update <- 
                  updateSummary(current = data, inputs = input) %>% 
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
                
                # If no reduction occurs add a row that says 0 tpeChange for the if clause to work
                if(!any(changes$direction == 1)){
                  changes <-
                    changes %>% 
                    add_row(
                      direction = 1,
                      tpeChange = 0
                    )
                }
                
                if(input$playerType == "Outfield"){
                  # Gets current primary and secondary positions and their number
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
                  
                  # Gets current traits and their number
                  currentTraits <- data$traits %>% str_split(pattern = " \\\\ ", simplify = TRUE) %>% unlist()
                  nrTraits <- length(currentTraits)
                  
                  # If the current player is a GK they should get the base restrictions for a redistribution
                  if(data$pos_gk == 20){
                    nrTraits <- 2
                    posPrim <- 1
                    posSec <- 1:2
                  }
                  
                  if(length(input$primary) != length(posPrim) | length(input$secondary) != length(posSec)){
                    showToast("error", "Your primary and/or secondary positions does not match with what you are allowed to select.")
                  } else if(input$traits %>% length() != nrTraits) {
                    showToast("error", "You have selected the wrong number of traits.")
                  } else if(updating() == "redistributing" & (changes %>% filter(direction == 1) %>% select(tpeChange) > 100)){
                    showToast("error", "You have removed more than the allowed 100 TPE in the redistribution.")  
                  } else {
                    update <- 
                      update %>% 
                      mutate(
                        old = as.character(old),
                        new = as.character(new)
                      ) %>% 
                      add_row(
                        attribute = "traits",
                        old = data$traits,
                        new = paste0(input$traits, collapse = " \\\\ ")
                      )
                    
                    # Add pos_ variables for each position
                    positions <- c("GK", "LD", "CD", "RD", "LWB", "CDM", "RWB", "LM", "CM", "RM", "LAM", "CAM", "RAM", "ST")
                    for (pos in positions) {
                      update <- update %>%
                        add_row(
                          attribute = paste0("pos_", tolower(pos)),
                          old = data[,paste0("pos_", tolower(pos))] %>% as.character(),
                          new = if_else(any(input$primary == pos), 20, if_else(any(input$secondary == pos), 15, 0)) %>% as.character()
                        )
                    }
                    
                    update <- 
                      update %>% 
                      filter(
                        old != new
                      )
                    
                    if(nrow(update) > 0){
                      modalVerify(update, session)  
                    } else {
                      showToast("warning", "You haven't changed anything in your build.")
                    }
                    
                  }
                } else {
                  if(updating() == "redistributing" & (changes %>% filter(direction == 1) %>% select(tpeChange) > 100)){
                    showToast("error", "You have removed more than the allowed 100 TPE in the redistribution.")  
                  } else {
                    update <- 
                      update %>% 
                      mutate(
                        old = as.character(old),
                        new = as.character(new)
                      ) %>% 
                      add_row(
                        attribute = "traits",
                        old = data$traits,
                        new = "NO TRAITS"
                      )
                    
                    # Add pos_ variables for each position
                    positions <- c("LD", "CD", "RD", "LWB", "CDM", "RWB", "LM", "CM", "RM", "LAM", "CAM", "RAM", "ST")
                    for (pos in positions) {
                      update <- update %>%
                        add_row(
                          attribute = paste0("pos_", tolower(pos)),
                          old = data[,paste0("pos_", tolower(pos))] %>% as.character(),
                          new = 0 %>% as.character()
                        )
                    }
                    
                    update <- 
                      update %>% 
                      add_row(
                        attribute = "pos_gk",
                        old = data$pos_gk %>% as.character(),
                        new = 20 %>% as.character()
                      )
                    
                    update <- 
                      update %>% 
                      filter(
                        old != new
                      )
                    
                    if(nrow(update) > 0){
                      modalVerify(update, session)  
                    } else {
                      showToast("warning", "You haven't changed anything in your build.")
                    }
                  }
                }
              }
            }
          )
      }) %>% 
        bindEvent(
          input$verifyUpdate,
          ignoreInit = TRUE
        )
      
      #### REDISTRIBUTING ####
      observe({
        data() %>% 
          then(
            onFulfilled = function(data){
              shinyjs::hide("attributeOverview")
              shinyjs::show("attributeUpdate")
              shinyjs::hide("tpeButtons")
              shinyjs::show("buttonsUpdating")
              shinyjs::show("outfieldExtras")
              shinyjs::show("playerSelector")
              
              updating("redistributing")
              
              showModal(
                modalDialog(
                  "You may only submit one redistribution in your career. If you only want to update, please click the Back button.",
                  title="Redistribution limit!",
                  footer = modalButton("I understand!"),
                  easyClose = FALSE
                )
              )
              
              data %>% 
                select(acceleration:throwing) %>% 
                select(!c(`natural fitness`, stamina)) %>% 
                colnames() %>% 
                map(
                  .x = .,
                  .f = function(x){
                    updateNumericInput(
                      session = session,
                      inputId = x %>% stringr::str_to_title() %>% str_remove_all(pattern = " "),
                      value = data[, x],
                      min = 5,
                      max = 20
                    )
                  }
                )
              
              data %>% 
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
            }
          )
      }) %>% 
        bindEvent(
          input$goToRedist,
          ignoreInit = TRUE
        )
      
      #### REROLLING ####
      observe({
        data() %>% 
          then(
            onFulfilled = function(data){
              shinyjs::hide("attributeOverview")
              shinyjs::show("attributeUpdate")
              shinyjs::hide("tpeButtons")
              shinyjs::show("buttonsUpdating")
              shinyjs::show("outfieldExtras")
              shinyjs::show("playerSelector")
              
              updating("rerolling")
              
              showModal(
                modalDialog(
                  "You may only submit one reroll in your career. If you only want to update, please click the Back button.",
                  title="Reroll limit!",
                  footer = modalButton("I understand!"),
                  easyClose = FALSE
                )
              )
              
              data %>% 
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
              
              data %>% 
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
            }
          )
      }) %>% 
        bindEvent(
          input$goToReroll,
          ignoreInit = TRUE
        )
      
      #### REGRESSION ####
      observe({
        data() %>% 
          then(
            onFulfilled = function(data){
              shinyjs::hide("attributeOverview")
              shinyjs::show("attributeUpdate")
              shinyjs::show("buttonsRegression")
              shinyjs::hide("tpeButtons")
              
              updating("updating")
              
              data %>% 
                select(acceleration:throwing) %>% 
                select(!c(`natural fitness`, stamina)) %>% 
                colnames() %>% 
                map(
                  .x = .,
                  .f = function(x){
                    updateNumericInput(
                      session = session,
                      inputId = x %>% stringr::str_to_title() %>% str_remove_all(pattern = " "),
                      value = data[, x],
                      max = data[, x],
                      min = 5
                    )
                  }
                )
              
              data %>% 
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
            }
          )
      }) %>% 
        bindEvent(
          input$goToRegression,
          ignoreInit = TRUE
        )
      
      observe({
        data() %>% 
          then(
            onFulfilled = function(data){
              bank <- tpeBanked()
              
              if(bank < 0){
                showToast("error", "You have spent too much TPE on your attributes! Reduce some of your attributes and try again.")
              } else if(any(editableAttributes %>% sapply(X = ., FUN = function(att){input[[att]] > 20 | input[[att]] < 5}, simplify = TRUE))){
                showToast("error", "One or more of your attributes are lower than 5 or higher than 20, which exceeds the range of attributes we allow.")
              } else if(bank > 24){
                # Error shown if user has regressed too much
                showToast("error", "You have regressed too much. You may only remove up to 24 TPE more than the required regressed TPE.") 
              } else {
                update <- updateSummary(current = data, inputs = input)
                
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
            }
          )
      }) %>% 
        bindEvent(
          input$verifyRegression,
          ignoreInit = TRUE
        )
      
      #### CONFIRMING UPDATE ####
      observe({
        data() %>% 
          then(
            onFulfilled = function(data){
              # print("Confirming update")
              
              bank <- tpeBanked()
              
              removeModal()
              
              update <- updateSummary(current = data, inputs = input)
              
              if(updating() == "redistributing"){
                completeRedistribution(data$pid)
                
                if(input$playerType == "Outfield"){
                  update <- 
                    update %>% 
                    mutate(
                      old = as.character(old),
                      new = as.character(new)
                    ) %>% 
                    add_row(
                      attribute = "traits",
                      old = paste("'", data$traits, "'", sep = ""),
                      new = paste("'", paste0(input$traits, collapse = " \\\\ "), "'", sep = "")
                    )
                  
                  # Add pos_ variables for each position
                  positions <- c("GK", "LD", "CD", "RD", "LWB", "CDM", "RWB", "LM", "CM", "RM", "LAM", "CAM", "RAM", "ST")
                  for (pos in positions) {
                    update <- update %>%
                      add_row(
                        attribute = paste0("pos_", tolower(pos)),
                        old = data[,paste0("pos_", tolower(pos))] %>% as.character(),
                        new = if_else(any(input$primary == pos), 20, if_else(any(input$secondary == pos), 15, 0)) %>% as.character()
                      )
                  }
                  
                  update <- 
                    update %>% 
                    filter(
                      old != new
                    )
                } else {
                  update <- 
                    update %>% 
                    add_row(
                      attribute = "traits",
                      old = paste("'", data$traits, "'", sep = ""),
                      new = "'NO TRAITS'"
                    )
                  
                  # Add pos_ variables for each position
                  positions <- c("LD", "CD", "RD", "LWB", "CDM", "RWB", "LM", "CM", "RM", "LAM", "CAM", "RAM", "ST")
                  for (pos in positions) {
                    update <- update %>%
                      add_row(
                        attribute = paste0("pos_", tolower(pos)),
                        old = data[paste0("pos_", tolower(pos))],
                        new = 0
                      )
                  }
                  
                  update <- 
                    update %>% 
                    add_row(
                      attribute = "pos_gk",
                      old = data$pos_gk,
                      new = 20
                    )
                }
              }
              
              if(updating() == "rerolling"){
                completeReroll(data$pid)
                
                if(input$playerType == "Outfield"){
                  update <- 
                    update %>% 
                    mutate(
                      old = as.character(old),
                      new = as.character(new)
                    ) %>% 
                    add_row(
                      attribute = "traits",
                      old = paste("'", data$traits, "'", sep = ""),
                      new = paste("'", paste0(input$traits, collapse = " \\\\ "), "'", sep = "")
                    )
                  
                  # Add pos_ variables for each position
                  positions <- c("GK", "LD", "CD", "RD", "LWB", "CDM", "RWB", "LM", "CM", "RM", "LAM", "CAM", "RAM", "ST")
                  for (pos in positions) {
                    update <- update %>%
                      add_row(
                        attribute = paste0("pos_", tolower(pos)),
                        old = data[,paste0("pos_", tolower(pos))] %>% as.character(),
                        new = if_else(any(input$primary == pos), 20, if_else(any(input$secondary == pos), 15, 0)) %>% as.character()
                      )
                  }
                  
                } else {
                  update <- 
                    update %>% 
                    mutate(
                      old = as.character(old),
                      new = as.character(new)
                    ) %>%
                    add_row(
                      attribute = "traits",
                      old = paste("'", data$traits, "'", sep = ""),
                      new = "'NO TRAITS'"
                    )
                  
                  # Add pos_ variables for each position
                  positions <- c("LD", "CD", "RD", "LWB", "CDM", "RWB", "LM", "CM", "RM", "LAM", "CAM", "RAM", "ST")
                  for (pos in positions) {
                    update <- update %>%
                      add_row(
                        attribute = paste0("pos_", tolower(pos)),
                        old = data[,paste0("pos_", tolower(pos))] %>% as.character(),
                        new = 0 %>% as.character()
                      )
                  }
                  
                  update <- 
                    update %>% 
                    add_row(
                      attribute = "pos_gk",
                      old = data$pos_gk %>% as.character(),
                      new = 20 %>% as.character()
                    )
                }
              }
              
              update <- 
                update %>% 
                filter(
                  old != new
                )
              
              updateLog(uid = uid, pid = data$pid, updates = update)
              
              updateBuild(pid = data$pid, updates = update, bank = bank)
              
              updated(updated() + 1)
              
              updating("")
              
              shinyjs::show("attributeOverview")
              shinyjs::hide("attributeUpdate")
              shinyjs::hide("buttonsUpdating")
              shinyjs::hide("buttonsRegression")
              shinyjs::show("tpeButtons")
              shinyjs::hide("outfieldExtras")
              shinyjs::hide("playerSelector")
              
              # print("Go back to overview from confirmation")
            }
          )
      }) %>% 
        bindEvent(
          input$confirmUpdate,
          ignoreInit = TRUE
        )
      
      #### OBSERVERS ####
      
      # Adds the box of positions and traits when selecting an outfield build
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
      
      # Updates the banked tpe when changing attributes
      observe({data() %>% 
          then(
            onFulfilled = function(data){
              if(updating() != ""){
                tpeBanked(
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
                      data$tpe - .
                    }
                )
              }
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
          ignoreInit = TRUE
        )
      
      # Resets the player build 
      observe({
        data() %>% 
          then(
            onFulfilled = function(data){
              data %>% 
                select(acceleration:throwing) %>% 
                colnames() %>% 
                map(
                  .x = .,
                  .f = function(x){
                    updateNumericInput(
                      session = session,
                      inputId = x %>% stringr::str_to_title() %>% str_remove_all(pattern = " "),
                      value = data[, x]
                    )
                  }
                )
              
              data %>%
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
            }
          )
      }) %>% 
        bindEvent(
          combineTriggers(input$resetUpdate, input$resetRegression),
          ignoreInit = TRUE
        )
      
      # Adds a go back button for both updating and regression
      observe({
        data() %>% 
          then(
            onFulfilled = function(data){
              shinyjs::show("attributeOverview")
              shinyjs::hide("attributeUpdate")
              shinyjs::hide("buttonsUpdating")
              shinyjs::hide("buttonsRegression")
              shinyjs::show("tpeButtons")
              
              updateRadioButtons(session = session, "playerType", selected = if_else(data$pos_gk == 20, "Goalkeeper", "Outfield"))
              shinyjs::hide("playerSelector")
              
              tpeBanked(data %>% 
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
                          })
              
              updating("")
            }
          )
      }) %>% 
        bindEvent(
          combineTriggers(input$backUpdate, input$backRegression),
          ignoreInit = TRUE
        )
      
      # Fixes moving away from locked 20 in stamina and natural fitness
      observe({
        if(updating() != ""){
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
    }
  )
}
