playerEditUI <- function(id) {
  ns <- NS(id)
  tagList(
    tagList(
      fluidRow(
        column(width = 12,
               column(width = 3, 
                      uiOutput(ns("selectPlayer"))
                      ),
               column(
                 width = 6, 
                 uiOutput(ns("playerFilter"))  
               )
             )
      ),
      fluidRow(
        column(4, 
               box(
                 title = "Organization Info",
                 width = NULL,
                 uiOutput(ns("organization"))
               )
        ),
        column(8, 
               box(
                 title = "Player Info",
                 width = NULL,
                 uiOutput(ns("player"))
               )
        )
      ),
      fluidRow(
        column(12,
               box(
                 title = "Positions and Traits Info",
                 width = NULL,
                 uiOutput(ns("positions")),
                 uiOutput(ns("traits"))
               )
        )
      ),
      fluidRow(
        div(
          class = "frozen-bottom",
          actionButton(
            inputId = ns("update"),
            label = "Update Player"
          )
        )
      ),
      div(style = "min-height:100px;")
    )
  )
}

playerEditServer <- function(id, uid) {
  moduleServer(
    id,
    function(input, output, session) {
      updated <- reactiveVal({0})
      
      #### REACTIVE DATA ####
      playerData <- reactive({
        req(input$selectedPlayer)
        pid <- input$selectedPlayer %>% as.numeric()
        
        readAPI(url = "https://api.simulationsoccer.com/player/getPlayer", query = list(pid = pid)) %>% 
          future_promise()
        
      }) %>% 
        bindEvent(
          input$selectedPlayer,
          updated()
        )
      
      allNames <- reactive({
        readAPI(url = "https://api.simulationsoccer.com/player/getAllPlayers") %>% 
          select(
            name, pid, username, team, status_p
          ) %>% 
          future_promise()
      })
      
      organizations <- reactive({
        readAPI("https://api.simulationsoccer.com/organization/getOrganizations") %>% 
          select(id = ID, name = organization, abbr = abbreviation) %>% 
          filter(!is.na(name)) %>% 
          unique() %>% 
          future_promise()
      })
      
      #### OUTPUTS ####
      output$selectPlayer <- renderUI({
        req(input$retired, input$freeAgent)
        allNames() %>% 
          then(
            onFulfilled = function(names) {
              names <- 
                names %>%
                filter(if(input$retired != 1) status_p > 0 else TRUE) %>%
                filter(if(input$freeAgent != 1) !(team %in% c("FA", "Retired")) else TRUE) %>% 
                arrange(name)
              
              namedVector <- names$pid
              
              names(namedVector) <- names$name
              
              selectInput(session$ns("selectedPlayer"), "Select Player", choices = namedVector)
            }
          )
      })
      
      output$playerFilter <- renderUI({
        tagList(
          column(
            width = 4,
            radioButtons(session$ns("retired"), label = "Include retired: ", choices = c("Yes" = 1, "No" = 0), inline = TRUE, selected = 0)  
          ),
          column(
            width = 4,
            radioButtons(session$ns("freeAgent"), label = "Include free agents: ", choices = c("Yes" = 1, "No" = 0), inline = TRUE)  
          )
        )
      })
      
      output$organization <- renderUI({
        promise_all(
          data = playerData(),
          organizations = organizations()
        ) %...>% 
          with({
            orgVector <- setNames(organizations$id, organizations$name)
            currentOrg <- data$organization
            
            # print(organizations)
            # print(currentOrg)
            
            tagList(
              selectInput(session$ns("organization"), label = "Select organization", choices = c("", orgVector), selected = orgVector[currentOrg]),
              selectInput(session$ns("affiliate"), label = "Select affiliate", choices = c("", "Major" = 1, "Minor" = 2), selected = data$affiliate)
            )
          })
      })
      
      output$player <- renderUI({
        playerData() %>% 
          then(
            onFulfilled = function(data){
              tagList(
                fluidRow(
                  column(6, 
                       selectInput(session$ns("position"), label = "Change preferred position", choices = names(positionsGK), selected = data$position)
                  ),
                  column(6, 
                       textInput(session$ns("render"), label = "Change render", value = data$render)
                  )
                ),
                fluidRow(
                  column(
                    width = 6,
                    numericInput(session$ns("left"), label = "Change Left Foot", min = data$`left foot`, value = data$`left foot`, max = 20, step = 5)  
                  ),
                  column(
                    width = 6,
                    numericInput(session$ns("right"), label = "Change Right Foot", min = data$`right foot`, value = data$`right foot`, max = 20, step = 5)  
                  )
                )
              )
            })
      })
      
      output$positions <- renderUI({
        playerData() %>% 
          then(
            onFulfilled = function(data){
              currentPositions <- 
                data %>% 
                select(pos_st:pos_gk) %>% 
                pivot_longer(everything()) %>% 
                mutate(name = name %>% str_remove_all("pos_") %>% str_to_upper())
              
              posPrim <- positionsGK[names(positionsGK) %in% (currentPositions %>% 
                                                            filter(value == 20) %>% 
                                                            select(name) %>% unlist())
              ]
              
              posSec <- positionsGK[names(positionsGK) %in% (currentPositions %>% 
                                                           filter(value < 20 & value > 5) %>% 
                                                           select(name) %>% unlist())
              ]
              
              posRem <- positionsGK[!(positionsGK %in% c(posPrim, posSec))]
              
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
            }
          )
      })
      
      
      output$traits <- renderUI({
        playerData() %>% 
          then(
            onFulfilled = function(data){
              currentTraits <- data$traits %>% str_split(pattern = traitSep) %>% unlist()
              
              tagList(
                checkboxGroupInput(
                  session$ns("traits"), 
                  paste("Change traits"), 
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
              )
            }
          )
        
      })
      
      
      #### OBSERVERS ####
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
      
      observe({
        playerData() %>% 
          then(
            onFulfilled = function(data){
              summary <- 
                tibble(
                  team = input$organization,
                  affiliate = input$affiliate,
                  position = input$position,
                  `left foot` = input$left,
                  `right foot` = input$right,
                  traits = paste0(input$traits, collapse = traitSep),
                  render = input$render
                ) 
              
              for(pos in names(positionsGK)){
                column <- paste0("pos_", str_to_lower(pos))
                
                summary <- 
                  summary %>% 
                  cbind(
                    tibble(
                      !!column := if_else(any(input$primary == pos), 20, if_else(any(input$secondary == pos), 15, 0))
                    )
                  )
              }
              
              old <- 
                data %>% 
                select(team = organization, affiliate, position, `left foot`, `right foot`, traits, pos_st:pos_gk, render) %>% 
                pivot_longer(
                  everything(),
                  values_transform = as.character
                )
              
              update <- 
                summary %>% 
                pivot_longer(everything(), values_transform = as.character) %>% 
                left_join(
                  old,
                  by = "name"
                ) %>% 
                rename(attribute = name, new = value.x, old = value.y) %>% 
                filter(old != new)
              
              updateLog(uid = uid, pid = data$pid, updates = update)

              updateBuild(pid = data$pid, updates = update)
              
              
              updated(updated() + 1)
              
              showToast(.options = myToastOptions,type = "success", "The player has been updated!")
            }
          )
      }) %>% 
        bindEvent(
          input$update
        )
      
    }
      
  )
}
