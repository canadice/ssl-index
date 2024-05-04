createPlayerUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        h3("How to use this tool?"),
        p("Welcome to the Player Creation Tool! This tool allows you to define all of the aspects of your player using
            text and selection prompts before combining all of the information ready for submission."),
        p("The cost for increasing an attribute is shown in the TPE table at the bottom of the page.")
      )
    ),
    ##### Player Details #####
    fluidRow( 
      column(
        width = 12,
        box(
          title = "Player Details and Cosmetics",
          solidHeader = TRUE,
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
                          choices = c("--- Select your nationality ---", "Other", "Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda", "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei", "Bulgaria", "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Canada", "Central African Republic", "Chad", "Chile", "China", "Colombia", "Comoros", "Côte d'Ivoire", "Democratic Republic of the Congo", "Republic of the Congo", "Costa Rica", "Croatia", "Cuba", "Cyprus", "Czechia", "Denmark", "Djibouti", "Dominica", "Dominican Republic", "East Timor", "England", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Eswatini", "Ethiopia", "Faroe Islands", "Fiji", "Finland", "France", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Gibraltar", "Greece", "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Israel", "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Korea, North", "Korea, South", "Kosovo", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Mauritania", "Mauritius", "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands", "New Zealand", "Nicaragua", "Niger", "Nigeria", "North Macedonia", "Northern Ireland", "Norway", "Oman", "Pakistan", "Palau", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Qatar", "Romania", "Russia", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Sao Tome and Principe", "Saudi Arabia", "Scotland", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", "Sweden", "Switzerland", "Syria", "Taiwan", "Tajikistan", "Tanzania", "Thailand", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United States", "Uruguay", "Uzbekistan", "Vanuatu", "Vatican City", "Venezuela", "Vietnam", "Wales", "Yemen", "Zambia", "Zimbabwe"))
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
              selectInput(ns("footedness"), "Select preferred foot:", choices = c("--- Select a side ---", "Left", "Right"))
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
              selectInput(ns("hairColor"), "Select hair color:", choices = c("--- Select a color ---", "Light Brown 1", "Light Brown 2", "Light Brown 3", "Dark Brown 1", "Dark Brown 2", "Dark Brown 3", "Blond(e) 1", "Blond(e) 2", "Blond(e) 3", "Black 1", "Black 2", "Black 3", "Red 1", "Red 2", "Red 3"))
            ),
            column(
              width = 4,
              selectInput(ns("hairLength"), "Select hair length:", choices = c("--- Select a length ---", "Bald", "Buzzcut", "Short", "Medium", "Long"))
            ),
            column(
              width = 4,
              selectInput(ns("skinColor"), "Select skin tone:", choices = c("--- Select a skin tone ---", "Skin Tone 1 (lightest)", "Skin Tone 2", "Skin Tone 3", "Skin Tone 4", "Skin Tone 5", "Skin Tone 6", "Skin Tone 7", "Skin Tone 8", "Skin Tone 9", "Skin Tone 10", "Skin Tone 11", "Skin Tone 12", "Skin Tone 13", "Skin Tone 14", "Skin Tone 15", "Skin Tone 16", "Skin Tone 17", "Skin Tone 18", "Skin Tone 19", "Skin Tone 20 (darkest)"))
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
        status = "primary",
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
        ),
        hr(),
        ##--------------------------
        ##  Player Traits  
        ##--------------------------
        fluidRow(
          column(
            width = 6,
            p("An outfield player may select one primary position and two secondary positions."),
            uiOutput(
              outputId = ns("positionSelector")
            )
          ),
          column(
            width = 6,
            p("An outfield player may select two traits to start off their career."),
            uiOutput(
              outputId = ns("traitSelector")
            )
          )
        ) %>% 
          div(id = ns("outfieldExtras"))
      ) %>% column(width = 12)
    ),
    ##### Submission #####
    fluidRow(
      column(
        width = 12,
        box(
          title = "Submission",
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          h5("Update Scale" %>% strong(), align = "center"),
          reactableOutput(
            outputId = ns("costTPE"),
            width = "80%"
          ) %>% 
            div(align = "center"),
          actionButton(
            inputId = ns("submitButton"),
            label = "Submit Player",
            width = "30%"
          ) %>% 
            div(align = "center")
        )
      )
    )
  )
}

createPlayerServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      editableAttributes <- 
        attributes$attribute %>% 
        .[!(. %in% c("Natural Fitness", "Stamina"))] %>% 
        str_to_title() %>% 
        str_remove_all(pattern = " ") 
      
      
      # Define position options
      positions <- c(
        "Left Defender", "Central Defender", "Right Defender",
        "Left Wing Back", "Defensive Midfielder", "Right Wing Back",
        "Left Midfielder", "Central Midfielder", "Right Midfielder",
        "Left Attacking Midfielder", "Central Attacking Midfielder", "Right Attacking Midfielder",
        "Striker"
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
          c(
            "Aerial Reach", "Command Of Area", "Communication", 
            "Eccentricity", "Handling", "Kicking", "One On Ones", 
            "Tendency To Punch", "Reflexes", "Tendency To Rush", "Throwing",
            "Free Kick", "Penalty Taking", "Technique"
          ) %>% 
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
          selectInput(session$ns("posPrim"), withTooltip("Primary Position:", "Converts to 20 positional experience in the position"), choices = c("", positions)),
          selectInput(session$ns("posSec1"), withTooltip("Secondary Position:", "Converts to 15 positional experience in the position"), choices = c("", positions)),
          selectInput(session$ns("posSec2"), withTooltip("Secondary Position:", "Converts to 15 positional experience in the position"), choices = c("", positions))
        )
      })
      
      # Dynamic UI for trait selectors
      output$traitSelector <- renderUI({
        tagList(
          selectInput(session$ns("trait1"), "Select Trait 1:", choices = traits),
          selectInput(session$ns("trait2"), "Select Trait 2:", choices = traits)
        )
      })
      
    }
  )
}