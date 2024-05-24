createPlayerUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        h3("How to use this tool?"),
        p("Welcome to the Player Creation Tool! This tool allows you to define all of the aspects of your player using
            text and selection prompts before combining all of the information ready for submission.")
      )
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
          width = NULL,
          fluidRow(
            column(
              width = 12, align = "center", style = "display: flex; justify-content: center;",
              valueBox(
                subtitle = "Total Earned TPE",
                value = 350,
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
        status = "info",
        width = NULL,
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
    fluidRow(
      actionButton(
        inputId = ns("submitButton"),
        label = "Submit Player",
        width = "30%"
      ) %>% 
        div(align = "center")
    )
  )
}

createPlayerServer <- function(id, userinfo, parent) {
  moduleServer(
    id,
    function(input, output, session) {
      # Define position options
      positions <- c(
        "LD" = "Left Defender", "CD" = "Central Defender", "RD" = "Right Defender",
        "LWB" = "Left Wing Back", "CDM" = "Defensive Midfielder", "RWB" = "Right Wing Back",
        "LM" = "Left Midfielder", "CM" = "Central Midfielder", "RM" = "Right Midfielder",
        "LAM" = "Left Attacking Midfielder", "CAM" = "Central Attacking Midfielder", 
        "RAM" = "Right Attacking Midfielder", "ST" = "Striker"
      )
      # Define trait options with optgroup
      traits <- list(
        "Movement - On the Ball" = list(
          "Cuts Inside From Both Wings", "Knocks Ball Past Opponent", "Runs With Ball Rarely",
          "Runs With Ball Often", "Runs With Ball Down Left", "Runs With Ball Down Right",
          "Runs With Ball Through Centre", "Stops Play", "Brings Ball Out Of Defense"
        ),
        "Movement - Off the Ball" =list(
          "Arrives Late In Opponent's Area", "Comes Deep To Get Ball", "Gets Forward Whenever Possible",
          "Gets Into Opposition Area", "Hugs Line", "Likes To Try To Beat Offside Trap",
          "Moves Into Channels", "Plays One-Twos", "Plays With Back To Goal",
          "Does Not Move Into Channels", "Stays Back At All Times"
        ),
        "Passing" = list(
          "Dictates Tempo", "Likes To Switch Ball To Other Flank", "Looks For Pass Rather Than Attempting To Score",
          "Plays No Through Balls", "Plays Short Simple Passes", "Tries Killer Balls Often",
          "Tries Long Range Passes", "Crossing Early"
        ),
        "Finishing" = list(
          "Attempts Overhead Kicks", "Hits Free Kicks With Power",
          "Likes To Lob Keeper", "Likes To Round Keeper", "Places Shots", "Refrains From Taking Long Shots",
          "Shoots From Distance", "Shoots With Power", "Tries First Time Shots", "Tries Long Range Free Kicks"
        ),
        "Discipline" = list(
          "Argues With Officials", "Winds Up Opponents", "Gets Crowd Going"
        ),
        "Defending" = list(
          "Dives Into Tackles", "Does Not Dive Into Tackles", "Marks Opponent Tightly"
        ),
        "Technique" = list(
          "Avoids Using Weaker Foot", "Uses Outside Of Foot", "Tries Tricks", "Likes To Beat Opponent Repeatedly",
          "Curls Ball", "Possess Long Flat Throw", "Tries To Play Way Out Of Trouble"
        )
      )
      
      tpeBanked <- 
        reactiveVal({350}) 
      
      output$tpeRemaining <- renderText({
        tpeBanked()
      })
      
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
              350 - .
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
        if(checkIfAlreadyApproving(userinfo$uid)) {
          showToast("error", "You already have a player waiting for approval. You cannot submit another one.")
        } else if(sapply(c(input$lastName, input$nationality, input$footedness, input$hairColor, input$hairLength, input$skinTone), FUN = function(x) x == "", simplify = TRUE) %>% any() | 
           (input$playerType == "Outfielder" & (input$traits %>% length() != 2 | input$primary %>% length() != 1 | input$secondary %>% length() != 2))){
          
          showToast("error", "You have missed to input some information. Please take a look at the highlighted inputs and correct the issues.")
          
          feedbackDanger("lastName", input$lastName == "", "Please enter at least a last name for your player. If you only want to use one name, please enter it here instead of as a first name.")
          feedbackDanger("nationality", input$nationality == "", "Please enter the nationality of your player.")
          feedbackDanger("footedness", input$footedness == "", "Please enter the footedness of your player.")
          feedbackDanger("hairColor", input$hairColor == "", "Please enter the hair color for your player.")
          feedbackDanger("hairLength", input$hairLength == "", "Please enter the hair length for your player.")
          feedbackDanger("skinTone", input$skinTone == "", "Please enter the skin tone for your player.")
          
          if(input$traits %>% length() != 2){
            showToast("error", "Please select two (2) traits.")
          }
          
          if(input$primary %>% length() != 1){
            showToast("error", "You can select one (1) primary position.")
          }
          
          if(input$secondary %>% length() != 2){
            showToast("error", "You can select two (2) secondary positions.")
          }
          
        } else if(checkDuplicatedName(input$firstName, input$lastName)){
          showToast("error", "Another player in the league's history have used this name. Please change it to something else.")
        } else if(any(editableAttributes %>% sapply(X = ., FUN = function(att){input[[att]] > 20 | input[[att]] < 5}, simplify = TRUE))){
          showToast("error", "One or more of your attributes are lower than 5 or higher than 20, which exceeds the range of attributes we allow.")
        } else if(tpeBanked() < 0){
          showToast("error", "You have spent too much TPE. Please adjust and resubmit.")
        } else if(tpeBanked() > 100){
          showToast("error", "Please allocate as much of the TPE you are given as possible. If you need help with your build, reach out to an Academy coach on Discord.")
        } else {
          checkBuild(input, tpeBanked(), session)
        }
      }) %>% 
        bindEvent(
          input$submitButton
        )
      
      observe({
        showToast("success", "Your player has been submitted for approval. You will be notified via the forum or Discord by a member of the BoD when the approval has been completed or if there are any issues.")        
        
        removeModal()
        
        submitBuild(input, tpeBanked(), userinfo)
        
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