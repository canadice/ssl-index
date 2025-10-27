createPlayerUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        h3("How to use this tool?"),
        p("Welcome to the Player Creation Tool! This tool allows you to define all of the aspects of your player using
            text and selection prompts before combining all of the information ready for submission."),
        paste("To help you in your process, we have also created a", 
              tags$a("Player Compendium", href = "https://docs.google.com/document/d/1cp4OdU43nX8A7kbQVmOl89xZRD3l13voHcqLNrxFL4Q/edit?usp=sharing"), 
              "that includes detailed descriptions of the player attributes, which are important for different player positions and roles,",
              "as well as descriptions of player traits.") %>% HTML() %>% p()
      )
    ),
    #### Position Tracker ####
    fluidRow(
      column(12,
             box(
               title = "Position Tracker",
               status = "info",
               collapsible = TRUE,
               solidHeader = TRUE,
               collapsed = TRUE,
               width = NULL,
               column(width = 4,
                      p("The pitch shows the number of players that has the position as one of their Primary/Secondary positions."),
                      br(),
                      radioButtons(inputId = ns("activeStatus"),label = "Show only active players?",choices = c("Yes", "No"),selected = "Yes")),
               column(width = 8,imageOutput(outputId = ns("fieldImage"),height = 600))
             ))
    ),
    ##### Player Details #####
    fluidRow( 
      column(
        width = 12,
        box(
          title = "Player Details and Cosmetics",
          solidHeader = TRUE,
          collapsible = TRUE,
          status = "info",
          width = NULL,
          h4("Player Details", align = "center"),
          fluidRow(
            column(
              width = 4,
              textInput(ns("firstName"), "Enter a first name:", placeholder = "First Name")
            ),
            column(
              width = 4,
              textInput(ns("lastName"), "Enter a last name:", placeholder = "Last Name")
            ),
            column(
              width = 4,
              textInput(ns("discordUsername"), "Enter your Discord username:", placeholder = "Username")
            )
          ),
          fluidRow(
            column(
              width = 4,
              textInput(ns("birthplace"), withTooltip("Enter a place of birth:", "Only thematic"), placeholder = "City")
            ),
            column(
              width = 4,
              selectInput(ns("nationality"), withTooltip("Select your nationality:", "Defines your international team affiliation"), 
                          choices = sslNations)
            ),
            column(
              width = 4,
              numericInput(ns("height"), withTooltip("Enter height (inches):", "Only cosmetic"), value = 62, min = 55, max = 90)
            )
          ),
          fluidRow(
            column(
              width = 4,
              numericInput(ns("weight"), withTooltip("Enter weight (pounds):", "Only cosmetic"), value = 180, min = 100, max = 350, step = 5)
            ),
            column(
              width = 4,
              selectInput(ns("footedness"), "Select preferred foot:", choices = c("", "Left", "Right"))
            ),
            column(
              width = 4,
              textInput(ns("render"), withTooltip("Select a player render:", "A person in the real world you would like your player to look like. (Optional for graphics used within the league)"), placeholder = "None")
            )
          ),
          fluidRow(
            column(
              width = 12,
              h4("FM Cosmetics", align = "center")
            ),
            column(
              width = 4,
              selectInput(ns("hairColor"), "Select hair color:", choices = c(
                "",
                "Light Brown 1" = "LBR1",
                "Light Brown 2" = "LBR2",
                "Light Brown 3" = "LBR3",
                "Dark Brown 1" = "DBR1",
                "Dark Brown 2" = "DBR2",
                "Dark Brown 3" = "DBR3",
                "Blond(e) 1" = "BLN1",
                "Blond(e) 2" = "BLN2",
                "Blond(e) 3" = "BLN3",
                "Black 1" = "BLK1",
                "Black 2" = "BLK2",
                "Black 3" = "BLK3",
                "Red 1" = "RED1",
                "Red 2" = "RED2",
                "Red 3" = "RED3"
              ))
            ),
            column(
              width = 4,
              selectInput(ns("hairLength"), "Select hair length:", choices = c("", "Bald", "Buzzcut", "Short", "Medium", "Long"))
            ),
            column(
              width = 4,
              selectInput(ns("skinColor"), "Select skin tone:", choices = c(
                "",
                "Skin Tone 1 (lightest)" = "1",
                "Skin Tone 2" = "2",
                "Skin Tone 3" = "3",
                "Skin Tone 4" = "4",
                "Skin Tone 5" = "5",
                "Skin Tone 6" = "6",
                "Skin Tone 7" = "7",
                "Skin Tone 8" = "8",
                "Skin Tone 9" = "9",
                "Skin Tone 10" = "10",
                "Skin Tone 11" = "11",
                "Skin Tone 12" = "12",
                "Skin Tone 13" = "13",
                "Skin Tone 14" = "14",
                "Skin Tone 15" = "15",
                "Skin Tone 16" = "16",
                "Skin Tone 17" = "17",
                "Skin Tone 18" = "18",
                "Skin Tone 19" = "19",
                "Skin Tone 20 (darkest)" = "20"
              ))
            )
          )
        )
      )
    ),
    ##### TPE Information #####
    fluidRow(
      column(
        width = 12,
        box(
          title = "General Information",
          status = "info",
          collapsible = TRUE,
          solidHeader = TRUE,
          collapsed = TRUE,
          width = NULL,
          fluidRow(
            column(
              width = 12, align = "center", style = "display: flex; justify-content: center;",
              valueBox(
                subtitle = "Total Earned TPE",
                value = 250,
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
          radioButtons(
            inputId = ns("playerType"),
            label = "Outfield or Goalkeeper",
            choices = c("Outfield", "Goalkeeper"),
            selected = "Outfield"
          ) %>% 
            div(align = "center")
        )
      )
    ),
    ##### Attributes #####
    fluidRow(
      box(
        title = "Player Attributes",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        status = "info",
        width = NULL,
        fluidRow(
          ## Selects player roles from a dropdown
          uiOutput(ns("roleSelector")) %>% column(width = 12),
        ),
        fluidRow(
          ##---------------------
          ##  Physical Attributes  
          ##---------------------
          column(
            style = "padding-right: 20px; padding-left: 20px",
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
                    ~ attributeInput(ns = ns, name = .x, value = 5)
                )
            ),
          ),
          ##---------------------
          ##  Mental Attributes  
          ##---------------------
          column(
            style = "padding-right: 20px; padding-left: 20px",
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
                  ~ attributeInput(ns = ns, name = .x, value = 5)
              ),
          ),
          ##---------------------
          ##  Technical Attributes  
          ##---------------------
          column(
            style = "padding-right: 20px; padding-left: 20px",
            width = 4,
            h4("Technical " %>% strong(), align = "center"),
            uiOutput(ns("technical")),
          )
        )
      ) %>% column(width = 12)
    ),
    ##--------------------------
    ##  Player Traits  
    ##--------------------------
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
            p("An outfield player may select two traits to start off their career."),
            uiOutput(
              outputId = ns("traitSelector")
            )
          )
        )
      ) %>% 
        column(width = 12)
    ) %>% 
      div(id = ns("outfieldExtras")),
    tags$script(paste("
        Shiny.addCustomMessageHandler('disableCheckbox', function(checkboxId) {
          if (typeof checkboxId === 'string') {
            checkboxId = [checkboxId]; // Convert single string to array
          }
          var checkboxes = document.getElementsByName('", ns('traits'), "');
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
      ", sep = "") %>% HTML()),
    div(style = "min-height: 100px;"),
    fluidRow(
      div(
        class = "frozen-bottom",
        div(h4("TPE Remaining: "), 
        textOutput(ns("tpeRemaining2"), inline = TRUE) %>% 
          h4()
        ),
        actionButton(
          inputId = ns("submitButton"),
          label = "Submit Player"
        )
      )
    )
  )
}

createPlayerServer <- function(id, userinfo, parent) {
  moduleServer(
    id,
    function(input, output, session) {
      
      tpeBanked <- 
        reactiveVal({250}) 
      
      output$tpeRemaining <- output$tpeRemaining2 <- renderText({
        tpeBanked()
      })
      
      #### POSITION TRACKER OUTPUT ####
      trackerData <- reactive({
        getAllPlayerPositions() %>% 
          then(
            onFulfilled = function(data){
              if(input$activeStatus == "Yes"){
                data <- 
                  data %>% 
                  filter(
                    status == "Active"
                  )  
              }
              
              data %>% 
                pivot_longer(
                  !status,
                  names_to = "posExp",
                  values_to = "Value"
                ) %>% 
                cbind(
                  positionalCoord,
                  .
                ) %>% 
                filter(
                  Value != 0
                ) %>% 
                group_by(position) %>% 
                summarize(
                  x = mean(x),
                  y = mean(y),
                  primary = sum(Value == 20),
                  secondary = sum(Value == 15)
                ) %>% 
                ungroup()
            }
          )
        
        
        
          
      })
      
      output$fieldImage <- renderImage({
        trackerData() %>% 
          then(
            onFulfilled = function(data){
              base <- pitch %>% image_ggplot()
              
              p <- 
                base + 
                geom_text(
                  mapping = aes(x = x, y = y),
                  data = data,
                  label = data$primary,
                  nudge_x = -35,
                  size = 8,
                  fontface = "bold"
                ) + 
                geom_text(
                  mapping = aes(x = x, y = y),
                  data = data,
                  label = "/",
                  size = 8,
                  fontface = "bold"
                ) + 
                geom_text(
                  mapping = aes(x = x, y = y),
                  data = data,
                  label = data$secondary,
                  nudge_x = 35,
                  size = 8,
                  fontface = "bold"
                ) +
                geom_rect(
                  aes(xmin = 60, xmax = 200, ymin = 100, ymax = 370),
                  fill = NA,
                  color = "black",
                  linetype = 2
                ) + 
                geom_rect(
                  aes(xmin = 550, xmax = 690, ymin = 100, ymax = 370),
                  fill = NA,
                  color = "black",
                  linetype = 2
                ) +
                geom_rect(
                  aes(xmin = 60, xmax = 200, ymin = 410, ymax = 680),
                  fill = NA,
                  color = "black",
                  linetype = 2
                ) + 
                geom_rect(
                  aes(xmin = 550, xmax = 690, ymin = 410, ymax = 680),
                  fill = NA,
                  color = "black",
                  linetype = 2
                ) +
                geom_text(
                  mapping = aes(x = x, y = y),
                  data = positionalCoord,
                  nudge_y = 35,
                  label = positionalCoord$position,
                  size = 4,
                  fontface = "italic",
                  color = "#00044D"
                ) 
              
              card <- image_graph(res = 96)
              print(
                p + 
                  theme(
                    legend.position = "none"
                  )
              )
              dev.off()
              
              tempImage <- 
                card %>% 
                image_crop(geometry = "480x600+160") %>% 
                image_write(tempfile(fileext = "png"), format = "png")
              
              return(
                list(
                  src = tempImage, 
                  contentType = "image/png"
                )
              )
            }
          )
        
        
      },
      deleteFile = TRUE
      )
      
      #### ROLE OUTPUT ####
      # Dynamic UI for role selector
      output$roleSelector <- renderUI({
        tagList(
          paste("Football Manager uses <i>roles</i> and <i>duties</i> to control what your player will do within a
        set tactic. There exists many different roles with different importances given to specific
        attributes. Selecting a role and duty in the list below will highlight the <span
        class='keyAttribute'>very important</span> and
        <span class='importantAttribute'>important</span>          attributes.") %>% HTML(),
          br(),
          selectInput(session$ns("selectedRole"), label = tippy("Select a player role", tooltip = "Please note, the role you play will be determined by your Manager. If you want to play a specific role, make sure to speak with your Manager."), choices = names(roleAttributes))
        )
      })
      
      #### ATTRIBUTE HIGHLIGHTS BASED ON ROLE ####
      observe({
        editableAttributes %>% 
          lapply(
            X = .,
            FUN = function(x){
              if(roleAttributes[[input$selectedRole]][[x]] == 1){
                feedback(
                  session = session,
                  show = TRUE,
                  inputId = x %>% stringr::str_to_title() %>% str_remove_all(pattern = " "),
                  color = importantColor,
                  icon = shiny::icon("exclamation-sign", lib = "glyphicon")
                ) 
              } else if(roleAttributes[[input$selectedRole]][[x]] == 2){
                feedback(
                  session = session,
                  show = TRUE,
                  inputId = x %>% stringr::str_to_title() %>% str_remove_all(pattern = " "),
                  color = keyColor,
                  icon = shiny::icon("exclamation-sign", lib = "glyphicon")
                )
              } else {
                hideFeedback(
                  session = session,
                  inputId = x %>% stringr::str_to_title() %>% str_remove_all(pattern = " ")
                )
              }
              
            }
          )
      }) %>% bindEvent(input$selectedRole)
      
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
      
      # Updates the banked tpe when changing attributes
      observe({
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
              250 - .
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
      
      # Fixes moving away from locked 20 in stamina and natural fitness
      observe({
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
      }) %>% 
        bindEvent(
          # Changes in any input slider
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
      
      observe({
        if(input$playerType == "Outfield"){
          shinyjs::show("outfieldExtras")
          
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
            map(
              .x = .,
              .f = 
                ~ attributeInput(ns = session$ns, name = .x, value = 5)
            )
        } else {
          shinyjs::hide("outfieldExtras")
          
          c(
            "Corners", "Crossing", "Dribbling", "Finishing", "First Touch",
            "Free Kick", "Heading", "Long Shots", "Long Throws", "Marking",
            "Passing", "Penalty Taking", "Tackling", "Technique"
          ) %>% 
            map(
              .x = .,
              .f = 
                ~ attributeInput(ns = session$ns, name = .x, value = 5)
            )
        }
      }) %>% 
        bindEvent(
          input$playerType,
          ignoreInit = TRUE
        )
      
      output$technical <- renderUI({
        
        if(input$playerType == "Outfield"){
          c(
            "Corners", "Crossing", "Dribbling", "Finishing", "First Touch",
            "Free Kick", "Heading", "Long Shots", "Long Throws", "Marking",
            "Passing", "Penalty Taking", "Tackling", "Technique"
          ) %>% 
            map(
              .x = .,
              .f = 
                ~ attributeInput(ns = session$ns, name = .x, value = NA)
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
            map(
              .x = .,
              .f = 
                ~ attributeInput(ns = session$ns, name = .x, value = NA)
            )
          
        }
      })
      
      # Dynamic UI for position selector
      output$positionSelector <- renderUI({
        tagList(
          
          bucket_list(
            header = "An outfield player may select one primary position and two secondary positions.",
            group_name = session$ns("pos"),
            orientation = "horizontal",
            add_rank_list(
              text = withTooltip("Select one primary position:", "Converts to 20 positional experience in the position"),
              labels = NULL,
              input_id = session$ns("primary")
            ),
            add_rank_list(
              text = withTooltip("Select two secondary positions:", "Converts to 15 positional experience in the position"),
              labels = NULL,
              input_id = session$ns("secondary")
            ),
            add_rank_list(
              text = "Drag from here",
              labels = positions,
              input_id = session$ns("unusedPositions")
            )
          )
        )
      })
      
      # Dynamic UI for trait selectors
      output$traitSelector <- renderUI({
        tagList(
          checkboxGroupInput(session$ns("traits"), "Select two (2) traits:", choices = traits %>% unlist(use.names = FALSE)) %>% 
            div(class = "multicol")
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
          ignoreInit = TRUE,
          ignoreNULL = FALSE
        )
      
      
      observe({
        # editableAttributes %>% sapply(X = ., FUN = function(att){input[[att]] > 20 | input[[att]] < 5}, simplify = TRUE) %>% unlist() %>% print()
        
        if(checkIfAlreadyApproving(userinfo$uid)) {
          showToast(.options = myToastOptions,"error", "You already have a player waiting for approval. You cannot submit another one.")
        } else if(sapply(c(input$lastName, input$nationality, input$footedness, input$hairColor, input$hairLength, input$skinColor), FUN = function(x) x == "", simplify = TRUE) %>% any() | 
           (input$playerType == "Outfield" & (input$traits %>% length() != 2 | input$primary %>% length() != 1 | input$secondary %>% length() != 2))){
          
          showToast(.options = myToastOptions,"error", "You have missed to input some information. Please take a look at the highlighted inputs and correct the issues.")
          
          feedbackDanger("lastName", input$lastName == "", "Please enter at least a last name for your player. If you only want to use one name, please enter it here instead of as a first name.")
          feedbackDanger("nationality", input$nationality == "", "Please enter the nationality of your player.")
          feedbackDanger("footedness", input$footedness == "", "Please enter the footedness of your player.")
          feedbackDanger("hairColor", input$hairColor == "", "Please enter the hair color for your player.")
          feedbackDanger("hairLength", input$hairLength == "", "Please enter the hair length for your player.")
          feedbackDanger("skinColor", input$skinColor == "", "Please enter the skin tone for your player.")
          
          if(input$traits %>% length() != 2){
            showToast(.options = myToastOptions,"error", "Please select two (2) traits.")
          }
          
          if(input$primary %>% length() != 1){
            showToast(.options = myToastOptions,"error", "You can select one (1) primary position.")
          }
          
          if(input$secondary %>% length() != 2){
            showToast(.options = myToastOptions,"error", "You can select two (2) secondary positions.")
          }
          
        } else if (input$weight < 100 | input$weight > 350 | input$height < 55 | input$height > 90){
          if(input$weight < 100 | input$weight > 350){
            showToast(.options = myToastOptions,"error", "The player's weight is outside of the allowed limits.")
            feedbackDanger("weight", TRUE, "The player's weight needs to be between 100 and 350 pounds.")
          }
          
          if(input$height < 55 | input$height > 90){
            showToast(.options = myToastOptions,"error", "The player's height is outside of the allowed limits.")
            feedbackDanger("height", TRUE, "The player's height needs to be between 55 and 90 inches.")
          }
          
        } else if(checkDuplicatedName(input$firstName, input$lastName)){
          showToast(.options = myToastOptions,"error", "Another player in the league's history have used this name. Please change it to something else.")
        } else if(editableAttributes %>% sapply(X = ., FUN = function(att){input[[att]] > 20 | input[[att]] < 5}, simplify = TRUE) %>% unlist() %>% any()){
          showToast(.options = myToastOptions,"error", "One or more of your attributes are lower than 5 or higher than 20, which exceeds the range of attributes we allow.")
        } else if(tpeBanked() < 0){
          showToast(.options = myToastOptions,"error", "You have spent too much TPE. Please adjust and resubmit.")
        } else if(tpeBanked() > 100){
          showToast(.options = myToastOptions,"error", "Please allocate as much of the TPE you are given as possible. If you need help with your build, reach out to an Academy coach on Discord.")
        } else {
          checkBuild(input, tpeBanked(), session)
        }
      }) %>% 
        bindEvent(
          input$submitButton
        )
      
      observe({
        removeModal()
        
        submitBuild(input, tpeBanked(), userinfo)
        
        showToast(.options = myToastOptions,"success", "Your player has been submitted for approval. You will be notified via the forum or Discord by a member of the BoD when the approval has been completed or if there are any issues.")        
        
        updateTabItems(parent, "tabs", "welcome")
      }) %>% 
        bindEvent(
          input$confirmBuild,
          ignoreInit = TRUE,
          once = TRUE
        )
      
      
    }
  )
}
