
###########################################################################
###########################################################################
###                                                                     ###
###                         PLAYER BUILDER TOOL                         ###
###                                                                     ###
###########################################################################
###########################################################################


### UI module for player similarities using MDS
playerBuilderUI <- function(id){
  
  ## Creating the namespacing function for all iDs
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(
        
        tags$style(HTML(paste("#playerBuilder-", attributeNames, "{height: 21px}", sep = ""))),
        tags$style(HTML(paste("#playerBuilder-", goalieAttributeNames, "{height: 21px}", sep = ""))),
        
        fluidRow(
          ##-----------------------------------
          ##  Left hand side with information  
          ##-----------------------------------
          column(
            width = 4,
            box(
              title = "Information",
              status = "info",
              solidHeader = TRUE,
              width = NULL,
              selectInput(
                inputId = ns("playerType"),
                label = "Player or Goalkeeper?",
                choices = c("Player", "Goalkeeper"),
                selected = "Player"
              ),
              h4("Points Information", align = "center"),
              fluidRow(
                column(
                  width = 10,
                  offset = 1,
                  fluidRow(
                    column(
                      width = 6,
                      h5("Earned TPE")
                    ),
                    column(
                      width = 6,
                      numericInput(
                        inputId = ns("earnedTPE"),
                        label = NULL,
                        value = 350,
                        min = 350,
                        max = 2500,
                        width = "80%"
                      )
                    )
                  ),
                  uiOutput(
                    outputId = ns("usedTPE")
                  )
                )
              ),
              br(),
              h5("Update Scale" %>% strong(), align = "center"),
              DTOutput(
                outputId = ns("costTPE"),
                width = "80%"
              ) %>% 
                div(align = "center"),
              actionButton(
                inputId = ns("exportButton"),
                label = "Export",
                width = "80%"
              ) %>% 
                div(align = "center")
            )
          ),
          
          ##------------------------------------
          ##  Right hand side with the builder  
          ##------------------------------------
          
          column(
            width = 8,
            box(
              title = "Player Information and Cosmetics",
              solidHeader = TRUE,
              status = "success",
              width = NULL,
              h4("Player Details", align = "center"),
              fluidRow(
                column(
                  width = 4,
                  style = "margin-top: 21px;",
                  textInput(
                    inputId = ns("playerFirstName"),
                    label = "Write the First Name"
                  ),
                  numericInput(
                    inputId = ns("playerHeight"),
                    label = "Set Height in Inches",
                    value = 60,
                    min = 55,
                    max = 90,
                    step = 1
                  )
                ),
                column(
                  width = 4,
                  style = "margin-top: 21px;",
                  textInput(
                    inputId = ns("playerLastName"),
                    label = "Write the Last Name"
                  ),
                  numericInput(
                    inputId = ns("playerWeight"),
                    label = "Set Weight in Pounds",
                    value = 140,
                    min = 100,
                    max = 350,
                    step = 5
                  ),
                  selectInput(
                    inputId = ns("playerPosition"),
                    label = "Select Preferred Position",
                    choices = 
                      abilityMatrix$Attribute
                  )
                ),
                column(
                  width = 4,
                  textInput(
                    inputId = ns("playerBirthplace"),
                    label = "Write the Birthplace <br> (City and Country)" %>% HTML()
                  ),
                  selectInput(
                    inputId = ns("playerFoot"),
                    label = "Select Preferred Foot",
                    choices = 
                      c(
                        "Left",
                        "Right"
                      )
                  )
                )
              ),
              h4("Cosmetics", align = "center"),
              fluidRow(
                column(
                  width = 4,
                  style = "margin-top: 21px;",
                  selectInput(
                    inputId = ns("playerHairColor"),
                    label = "Select Hair Color",
                    choices = 
                      apply(
                        expand.grid(
                          c("Light Brown",
                            "Dark Brown",
                            "Black",
                            "Red",
                            "Blonde"),
                          1:3
                        ) %>% 
                          arrange(Var1),
                        1,
                        paste,
                        collapse = " "
                      )
                  )
                ),
                column(
                  width = 4,
                  style = "margin-top: 21px;",
                  selectInput(
                    inputId = ns("playerHairLength"),
                    label = "Select Hair Length",
                    choices = 
                      c(
                        "Bald",
                        "Buzzcut",
                        "Short",
                        "Medium",
                        "Long"
                      )
                  )
                ),
                column(
                  width = 4,
                  selectInput(
                    inputId = ns("playerSkinTone"),
                    label = "Select Skin Tone <br> (1 is lightest, 20 is darkest)" %>% HTML(),
                    choices = 
                      paste(
                        "Skin Tone",
                        1:20
                      )
                  )
                )
              )
            ),
            box(
              title = "Player Attributes",
              solidHeader = TRUE,
              status = "success",
              width = NULL,
              ##---------------------
              ##  Physical Attributes  
              ##---------------------
              column(
                style = "padding-right: 20px; padding-left: 20px",
                width = 4,
                h4("Physical " %>% strong(), align = "center"),
                fluidRow(
                  splitLayout(
                    "Attribute" %>% strong(),
                    "Value" %>% strong(),
                    "Cost" %>% strong(),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "padding: 10px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Acceleration",
                    numericInput(
                      inputId = ns("Acceleration"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costAcceleration")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Agility",
                    numericInput(
                      inputId = ns("Agility"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costAgility")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Balance",
                    numericInput(
                      inputId = ns("Balance"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costBalance")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Jumping Reach",
                    numericInput(
                      inputId = ns("JumpingReach"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costJumpingReach")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Natural Fitness*",
                    numericInput(
                      inputId = ns("NaturalFitness"),
                      label = NULL,
                      value = 20,
                      min = 20,
                      max = 20,
                      width = NULL
                    ) %>% disabled(),
                    uiOutput(outputId = ns("costNaturalFitness")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Pace",
                    numericInput(
                      inputId = ns("Pace"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costPace")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Stamina*",
                    numericInput(
                      inputId = ns("Stamina"),
                      label = NULL,
                      value = 20,
                      min = 20,
                      max = 20,
                      width = NULL
                    ) %>% disabled(),
                    uiOutput(outputId = ns("costStamina")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Strength",
                    numericInput(
                      inputId = ns("Strength"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costStrength")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                em("* indicates the value cannot be changed.") %>% span(align = "center")
              ),
              ##------------------------
              ##  Technical Attributes  
              ##------------------------
              column(
                width = 4,
                style = "padding-left: 20px",
                h4("Technical " %>% strong(), align = "center"),
                fluidRow(
                  splitLayout(
                    "Attribute" %>% strong(),
                    "Value" %>% strong(),
                    "Cost" %>% strong(),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "padding: 10px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Corners",
                    numericInput(
                      inputId = ns("Corners"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costCorners")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Crossing",
                    numericInput(
                      inputId = ns("Crossing"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costCrossing")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Dribbling",
                    numericInput(
                      inputId = ns("Dribbling"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costDribbling")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Finishing",
                    numericInput(
                      inputId = ns("Finishing"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costFinishing")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "First Touch",
                    numericInput(
                      inputId = ns("FirstTouch"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costFirstTouch")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Free Kick",
                    numericInput(
                      inputId = ns("FreeKick"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costFreeKick")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Heading",
                    numericInput(
                      inputId = ns("Heading"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costHeading")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Long Shots",
                    numericInput(
                      inputId = ns("LongShots"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costLongShots")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Long Throws",
                    numericInput(
                      inputId = ns("LongThrows"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costLongThrows")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Marking",
                    numericInput(
                      inputId = ns("Marking"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costMarking")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Passing",
                    numericInput(
                      inputId = ns("Passing"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costPassing")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Penalty Taking",
                    numericInput(
                      inputId = ns("PenaltyTaking"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costPenaltyTaking")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Tackling",
                    numericInput(
                      inputId = ns("Tackling"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costTackling")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Technique",
                    numericInput(
                      inputId = ns("Technique"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costTechnique")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                )
              ),
              ##---------------------
              ##  Mental Attributes  
              ##---------------------
              column(
                style = "padding-right: 25px",
                width = 4,
                h4("Mental " %>% strong(), align = "center"),
                fluidRow(
                  splitLayout(
                    "Attribute" %>% strong(),
                    "Value" %>% strong(),
                    "Cost" %>% strong(),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "padding: 10px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Aggression",
                    numericInput(
                      inputId = ns("Aggression"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costAggression")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "padding: 10px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Anticipation",
                    numericInput(
                      inputId = ns("Anticipation"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costAnticipation")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "padding: 10px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Bravery",
                    numericInput(
                      inputId = ns("Bravery"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costBravery")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "padding: 10px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Composure",
                    numericInput(
                      inputId = ns("Composure"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costComposure")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "padding: 10px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Concentration",
                    numericInput(
                      inputId = ns("Concentration"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costConcentration")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "padding: 10px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Decisions",
                    numericInput(
                      inputId = ns("Decisions"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costDecisions")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Determination",
                    numericInput(
                      inputId = ns("Determination"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costDetermination")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Flair",
                    numericInput(
                      inputId = ns("Flair"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costFlair")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Leadership",
                    numericInput(
                      inputId = ns("Leadership"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costLeadership")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Off the Ball",
                    numericInput(
                      inputId = ns("OffTheBall"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costOffTheBall")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Positioning",
                    numericInput(
                      inputId = ns("Positioning"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costPositioning")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Teamwork",
                    numericInput(
                      inputId = ns("Teamwork"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costTeamwork")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Vision",
                    numericInput(
                      inputId = ns("Vision"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costVision")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Work Rate",
                    numericInput(
                      inputId = ns("WorkRate"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("costWorkRate")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                )
              )
              ##------------------------
            ) %>% 
              div(id = ns("playerBuilder")),
            box(
              title = "Goalkeeper Attributes",
              solidHeader = TRUE,
              status = "success",
              width = NULL,
              ##---------------------
              ##  Mental Attributes  
              ##---------------------
              column(
                style = "padding-right: 25px",
                width = 4,
                h4("Mental " %>% strong(), align = "center"),
                fluidRow(
                  splitLayout(
                    "Attribute" %>% strong(),
                    "Value" %>% strong(),
                    "Cost" %>% strong(),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "padding: 10px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Aggression",
                    numericInput(
                      inputId = ns("GoalieAggression"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostAggression")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "padding: 10px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Anticipation",
                    numericInput(
                      inputId = ns("GoalieAnticipation"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostAnticipation")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "padding: 10px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Bravery",
                    numericInput(
                      inputId = ns("GoalieBravery"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostBravery")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "padding: 10px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Composure",
                    numericInput(
                      inputId = ns("GoalieComposure"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostComposure")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "padding: 10px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Concentration",
                    numericInput(
                      inputId = ns("GoalieConcentration"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostConcentration")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "padding: 10px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Decisions",
                    numericInput(
                      inputId = ns("GoalieDecisions"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostDecisions")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Determination",
                    numericInput(
                      inputId = ns("GoalieDetermination"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostDetermination")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Flair",
                    numericInput(
                      inputId = ns("GoalieFlair"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostFlair")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Leadership",
                    numericInput(
                      inputId = ns("GoalieLeadership"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostLeadership")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Off the Ball",
                    numericInput(
                      inputId = ns("GoalieOffTheBall"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostOffTheBall")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Positioning",
                    numericInput(
                      inputId = ns("GoaliePositioning"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostPositioning")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Teamwork",
                    numericInput(
                      inputId = ns("GoalieTeamwork"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostTeamwork")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Vision",
                    numericInput(
                      inputId = ns("GoalieVision"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostVision")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Work Rate",
                    numericInput(
                      inputId = ns("GoalieWorkRate"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostWorkRate")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                )
              ),
              ##---------------------
              ##  Physical Attributes  
              ##---------------------
              column(
                style = "padding-right: 20px; padding-left: 20px",
                width = 4,
                h4("Physical " %>% strong(), align = "center"),
                fluidRow(
                  splitLayout(
                    "Attribute" %>% strong(),
                    "Value" %>% strong(),
                    "Cost" %>% strong(),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "padding: 10px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Acceleration",
                    numericInput(
                      inputId = ns("GoalieAcceleration"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostAcceleration")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Agility",
                    numericInput(
                      inputId = ns("GoalieAgility"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostAgility")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Balance",
                    numericInput(
                      inputId = ns("GoalieBalance"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostBalance")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Jumping Reach",
                    numericInput(
                      inputId = ns("GoalieJumpingReach"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostJumpingReach")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Natural Fitness*",
                    numericInput(
                      inputId = ns("GoalieNaturalFitness"),
                      label = NULL,
                      value = 20,
                      min = 20,
                      max = 20,
                      width = NULL
                    ) %>% 
                      disabled(),
                    uiOutput(outputId = ns("GoaliecostNaturalFitness")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Pace",
                    numericInput(
                      inputId = ns("GoaliePace"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostPace")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Stamina*",
                    numericInput(
                      inputId = ns("GoalieStamina"),
                      label = NULL,
                      value = 20,
                      min = 20,
                      max = 20,
                      width = NULL
                    ) %>% 
                      disabled(),
                    uiOutput(outputId = ns("GoaliecostStamina")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Strength",
                    numericInput(
                      inputId = ns("GoalieStrength"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostStrength")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                em("* indicates the value cannot be changed.") %>% span(align = "center"),
                ##------------------------
                ##  Technical Attributes  
                ##------------------------
                h4("Technical " %>% strong(), align = "center"),
                fluidRow(
                  splitLayout(
                    "Attribute" %>% strong(),
                    "Value" %>% strong(),
                    "Cost" %>% strong(),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "padding: 10px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Free Kick",
                    numericInput(
                      inputId = ns("GoalieFreeKick"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostFreeKick")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Penalty Taking",
                    numericInput(
                      inputId = ns("GoaliePenaltyTaking"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostPenaltyTaking")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Technique",
                    numericInput(
                      inputId = ns("GoalieTechnique"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostTechnique")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                )
              ),
              ##--------------------------
              ##  Goalkeeping Attributes  
              ##--------------------------
              column(
                width = 4,
                style = "padding-left: 20px",
                h4("Goalkeeping " %>% strong(), align = "center"),
                fluidRow(
                  splitLayout(
                    "Attribute" %>% strong(),
                    "Value" %>% strong(),
                    "Cost" %>% strong(),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "padding: 10px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Aerial Reach",
                    numericInput(
                      inputId = ns("GoalieAerialReach"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostAerialReach")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Command <br> of Area" %>% HTML(),
                    numericInput(
                      inputId = ns("GoalieCommandOfArea"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostCommandOfArea")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px"), 
                    style = "margin-bottom: 5px"
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Communication",
                    numericInput(
                      inputId = ns("GoalieCommunication"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostCommunication")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Eccentricity",
                    numericInput(
                      inputId = ns("GoalieEccentricity"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostEccentricity")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "First Touch",
                    numericInput(
                      inputId = ns("GoalieFirstTouch"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostFirstTouch")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Handling",
                    numericInput(
                      inputId = ns("GoalieHandling"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostHandling")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Kicking",
                    numericInput(
                      inputId = ns("GoalieKicking"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostKicking")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "One on Ones",
                    numericInput(
                      inputId = ns("GoalieOneOnOnes"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostOneOnOnes")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Passing",
                    numericInput(
                      inputId = ns("GoaliePassing"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostPassing")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Tendency <br>to Punch" %>% HTML(),
                    numericInput(
                      inputId = ns("GoalieTendencyToPunch"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostTendencyToPunch")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px"),
                    style = "margin-bottom: 5px"
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Reflexes",
                    numericInput(
                      inputId = ns("GoalieReflexes"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostReflexes")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Tendency <br>to Rush" %>% HTML(),
                    numericInput(
                      inputId = ns("GoalieTendencyToRush"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostTendencyToRush")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px"), 
                    style = "margin-bottom: 5px"
                  )
                ),
                fluidRow(
                  splitLayout(
                    "Throwing",
                    numericInput(
                      inputId = ns("GoalieThrowing"),
                      label = NULL,
                      value = 5,
                      min = 5,
                      max = 20,
                      width = NULL
                    ),
                    uiOutput(outputId = ns("GoaliecostThrowing")) %>% span(align = "center"),
                    cellWidths = c("50%", "30%", "20%"),
                    callArgs = list(style = "text-align: right;padding: 4px")
                  )
                )
              )
              ##------------------------
            ) %>%
              div(id = ns("goalieBuilder")) %>% 
              hidden()
          )
        ),
        ##--------------------------
        ##  Player Traits  
        ##--------------------------
        fluidRow(
          column(
            width = 12,
            uiOutput(
              outputId = ns("playerTraits")
            )
          )
        )
        ##------------------------
      )
    )
 )
}

## Backend module for player similarities
playerBuilderSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){
      
      ### Creating the current build
      currentAvailable <- reactiveVal(350)
      
      currentCost <- reactiveVal(0)
      
      currentBuild <- reactive({
        if(input$playerType == "Player"){
          data.frame(
            values = 
              c(
                input$Aggression,
                input$Anticipation,
                input$Bravery,
                input$Composure,
                input$Concentration,
                input$Decisions,
                input$Determination,
                input$Flair,
                input$Leadership,
                input$OffTheBall,
                input$Positioning,
                input$Teamwork,
                input$Vision,
                input$WorkRate,
                input$Acceleration,
                input$Agility,
                input$Balance,
                input$JumpingReach,
                input$Pace,
                input$Strength,
                input$Corners,
                input$Crossing,
                input$Dribbling,
                input$Finishing,
                input$FirstTouch,
                input$FreeKick,
                input$Heading,
                input$LongShots,
                input$LongThrows,
                input$Marking,
                input$Passing,
                input$PenaltyTaking,
                input$Tackling,
                input$Technique
              )
          ) %>% 
            left_join(
              tpeCost,
              by = c("values" = "value")
            )
        } else {
          data.frame(
            values = 
              c(
                input$GoalieAggression,
                input$GoalieAnticipation,
                input$GoalieBravery,
                input$GoalieComposure,
                input$GoalieConcentration,
                input$GoalieDecisions,
                input$GoalieDetermination,
                input$GoalieFlair,
                input$GoalieLeadership,
                input$GoalieOffTheBall,
                input$GoaliePositioning,
                input$GoalieTeamwork,
                input$GoalieVision,
                input$GoalieWorkRate,
                input$GoalieAcceleration,
                input$GoalieAgility,
                input$GoalieBalance,
                input$GoalieJumpingReach,
                input$GoaliePace,
                input$GoalieStrength,
                input$GoalieFreeKick,
                input$GoaliePenaltyTaking,
                input$GoalieTechnique,
                input$GoalieAerialReach,
                input$GoalieCommandOfArea,
                input$GoalieCommunication,
                input$GoalieEccentricity,
                input$GoalieFirstTouch,
                input$GoalieHandling,
                input$GoalieKicking,
                input$GoalieOneOnOnes,
                input$GoaliePassing,
                input$GoalieTendencyToPunch,
                input$GoalieReflexes,
                input$GoalieTendencyToRush,
                input$GoalieThrowing
              )
          ) %>% 
            left_join(
              tpeCost,
              by = c("values" = "value")
            )
        }
        
      })
      
      reactives <- reactiveValues(
        positionalExperience =
          data.frame(
            "Position" = 
              c(
                "Striker:",
                "Attacking Midfielder [L]:",
                "Attacking Midfielder [C]:",
                "Attacking Midfielder [R]:",
                "Midfielder [L]:",
                "Midfielder [C]:",
                "Midfielder [R]:",
                "Wingback [L]:",
                "Defensive Midfielder [C]:",
                "Wingback [R]:",
                "Defense [L]:",
                "Defense [C]:",
                "Defense [R]:"
              ),
            "Experience" = 
              rep(0, times = 13)
          )
      )
      
      ### Observer that checks the type of player and shows the correct box
      observeEvent(
        input$playerType,
        {
          if(input$playerType == "Player"){
            hide(id = "goalieBuilder")  
            show(id = "playerBuilder")
          } else {
            show(id = "goalieBuilder")  
            hide(id = "playerBuilder")
          }
        },
        ignoreInit = TRUE
      )
      
      observeEvent(
        currentBuild() | input$earnedTPE,
        {
          currentAvailable(input$earnedTPE - sum(currentBuild()$cost))
          currentCost(sum(currentBuild()$cost))
        },
        ignoreInit = FALSE
      )
      
      ### Observer for the export button that creates a formatted text with all info of the build
      observeEvent(
        input$exportButton,
        {
          if(input$playerType == "Player"){
            ##---------------------------------------------------------------
            ##                        Player Export                         -
            ##---------------------------------------------------------------
            
            showModal(
              modalDialog(
                span(
                  "Copy the code below containing your created build into a forum post and fill out the EMPTY values."
                ),
                br(),
                br(),
                column(
                  width = 8,
                  offset = 2,
                  helpText(
                    paste(
                      "[size=7][u][b]Player Details[/b][/u][/size]<br>
                      Username: EMPTY<br>
                      First Name: ", input$playerFirstName, "<br>
                      Last Name: ", input$playerLastName, "<br>
                      Discord: EMPTY<br>
                      Birthplace: ", input$playerBirthplace, "<br>
                      Height: ", input$playerHeight, "<br>
                      Weight: ", input$playerWeight, "<br>
                      Preferred Foot: ", input$playerFoot, "<br>
                      Preferred Position: ", input$playerPosition, "<br>
                      <br>
                      [size=7][u][b]Cosmetics[/b][/u][/size]<br>
                      Hair Color: ", input$playerHairColor, "<br>
                      Hair Length: ", input$playerHairLength, "<br>
                      Skin Tone: ", input$playerSkinTone, "<br>
                      <br>
                      [size=7][u][b]Player Attributes[/b][/u][/size]<br>
                      TPE Available:",
                          currentAvailable(),
                          "<br><br>
                      [u][b]Physical[/b][/u]<br>
                      Acceleration:",
                          currentBuild()$value[15],
                          "<br>
                      Agility:",
                          currentBuild()$value[16],
                          "<br>
                      Balance:",
                          currentBuild()$value[17],
                          "<br>
                      Jumping Reach:", 
                          currentBuild()$value[18],
                          "<br>
                      Natural Fitness: 20<br>
                      Pace:", 
                          currentBuild()$value[19],
                          "<br>
                      Stamina: 20<br>
                      Strength:", 
                          currentBuild()$value[20],
                          "<br><br>
                      [u][b]Technical[/b][/u]<br>
                      Corners:", 
                          currentBuild()$value[21],
                          "<br>
                      Crossing:", 
                          currentBuild()$value[22],
                          "<br>
                      Dribbling:", 
                          currentBuild()$value[23],
                          "<br>
                      Finishing:", 
                          currentBuild()$value[24],
                          "<br>
                      First Touch:", 
                          currentBuild()$value[25],
                          "<br>
                      Free Kick:", 
                          currentBuild()$value[26],"<br>
                      Heading:", 
                          currentBuild()$value[27],"<br>
                      Long Shots:", 
                          currentBuild()$value[28],"<br>
                      Long Throws:", 
                          currentBuild()$value[29],"<br>
                      Marking:", 
                          currentBuild()$value[30],"<br>
                      Passing:", 
                          currentBuild()$value[31],"<br>
                      Penalty Taking:", 
                          currentBuild()$value[32],"<br>
                      Tackling:", 
                          currentBuild()$value[33],"<br>
                      Technique:", 
                          currentBuild()$value[34],"<br>
                      <br>
                      [u][b]Mental[/b][/u]<br>
                      Aggression:", 
                          currentBuild()$value[1],"<br>
                      Anticipation:", 
                          currentBuild()$value[2],"<br>
                      Bravery:", 
                          currentBuild()$value[3],"<br>
                      Composure:", 
                          currentBuild()$value[4],"<br>
                      Concentration:", 
                          currentBuild()$value[5],"<br>
                      Decisions:", 
                          currentBuild()$value[6],"<br>
                      Determination:", 
                          currentBuild()$value[7],"<br>
                      Flair:", 
                          currentBuild()$value[8],"<br>
                      Leadership:", 
                          currentBuild()$value[9],"<br>
                      Off the Ball:", 
                          currentBuild()$value[10],"<br>
                      Positioning:", 
                          currentBuild()$value[11],"<br>
                      Teamwork:", 
                          currentBuild()$value[12],"<br>
                      Vision:", 
                          currentBuild()$value[13],"<br>
                      Work Rate:", 
                          currentBuild()$value[14],"<br>
                      <br>
                      [u][b]Positional Experience[/b][/u]<br>",
                      paste(
                        reactives$positionalExperience$Position, 
                        reactives$positionalExperience$Experience
                      ) %>% 
                        paste(collapse = "<br>"),
                      "<br><br>
                      [u][b]Traits[/b][/u]<br>
                      Trait 1: ",
                      input$playerTraits[1],
                      "<br>
                      Trait 2: ", 
                      input$playerTraits[2]
                    ) %>% HTML()
                  ) %>% 
                    div(
                      style = "background: #f0f0f0; border: #656565"
                    )
                ) %>% 
                  fluidRow(),
                title="Build output",
                footer = 
                  tagList(
                    modalButton("Ok")
                  ),
                easyClose = TRUE
              )
            )
          } else {
            ##---------------------------------------------------------------
            ##                        Keeper Export                         -
            ##---------------------------------------------------------------
            showModal(
              modalDialog(
                span(
                  "Copy the code below containing your created build into a forum post and fill out the EMPTY values."
                ),
                br(),
                br(),
                column(
                  width = 8,
                  offset = 2,
                  helpText(
                    paste(
                      "[size=7][u][b]Player Details[/b][/u][/size]<br>
                      Username: EMPTY<br>
                      First Name: ", input$playerFirstName, "<br>
                      Last Name: ", input$playerLastName, "<br>
                      Discord: EMPTY<br>
                      Birthplace: ", input$playerBirthplace, "<br>
                      Height: ", input$playerHeight, "<br>
                      Weight: ", input$playerWeight, "<br>
                      Preferred Foot: ", input$playerFoot, "<br>
                      Preferred Position: ", input$playerPosition, "<br>
                      <br>
                      [size=7][u][b]Cosmetics[/b][/u][/size]<br>
                      Hair Color: ", input$playerHairColor, "<br>
                      Hair Length: ", input$playerHairLength, "<br>
                      Skin Tone: ", input$playerSkinTone, "<br>
                      <br>
                  [size=7][u][b]Player Attributes[/b][/u][/size]<br>
                  TPE Available:",
                      currentAvailable(),
                      "<br><br>
                  [u][b]Physical[/b][/u]<br>
                  Acceleration:",
                      currentBuild()$value[15],
                      "<br>
                  Agility:",
                      currentBuild()$value[16],
                      "<br>
                  Balance:",
                      currentBuild()$value[17],
                      "<br>
                  Jumping Reach:", 
                      currentBuild()$value[18],
                      "<br>
                  Natural Fitness: 20<br>
                  Pace:", 
                      currentBuild()$value[19],
                      "<br>
                  Stamina: 20<br>
                  Strength:", 
                      currentBuild()$value[20],
                      "<br><br>
                  [u][b]Technical[/b][/u]<br>
                  Free Kick:", 
                      currentBuild()$value[21],"<br>
                  Penalty Taking:", 
                      currentBuild()$value[22],"<br>
                  Technique:", 
                      currentBuild()$value[23],"<br>
                  <br>
                  [u][b]Mental[/b][/u]<br>
                  Aggression:", 
                      currentBuild()$value[1],"<br>
                  Anticipation:", 
                      currentBuild()$value[2],"<br>
                  Bravery:", 
                      currentBuild()$value[3],"<br>
                  Composure:", 
                      currentBuild()$value[4],"<br>
                  Concentration:", 
                      currentBuild()$value[5],"<br>
                  Decisions:", 
                      currentBuild()$value[6],"<br>
                  Determination:", 
                      currentBuild()$value[7],"<br>
                  Flair:", 
                      currentBuild()$value[8],"<br>
                  Leadership:", 
                      currentBuild()$value[9],"<br>
                  Off the Ball:", 
                      currentBuild()$value[10],"<br>
                  Positioning:", 
                      currentBuild()$value[11],"<br>
                  Teamwork:", 
                      currentBuild()$value[12],"<br>
                  Vision:", 
                      currentBuild()$value[13],"<br>
                  Work Rate:", 
                      currentBuild()$value[14],"<br>
                  <br>
                  [u][b]Goalkeeping[/b][/u]<br>
                  Aerial Reach:", 
                      currentBuild()$value[24],"<br>
                  Command of Area:", 
                      currentBuild()$value[25],"<br>
                  Communication:", 
                      currentBuild()$value[26],"<br>
                  Eccentricity:", 
                      currentBuild()$value[27],"<br>
                  First Touch:", 
                      currentBuild()$value[28],"<br>
                  Handling:", 
                      currentBuild()$value[29],"<br>
                  Kicking:", 
                      currentBuild()$value[30],"<br>
                  One on Ones:", 
                      currentBuild()$value[31],"<br>
                  Passing:", 
                      currentBuild()$value[32],"<br>
                  Tendency to Punch:", 
                      currentBuild()$value[33],"<br>
                  Reflexes:", 
                      currentBuild()$value[34],"<br>
                  Tendency to Rush:", 
                      currentBuild()$value[35],"<br>
                  Throwing:", 
                      currentBuild()$value[36]
                    ) %>% HTML()
                  ) %>% 
                    div(
                      style = "background: #f0f0f0; border: #656565"
                    )
                ) %>% 
                  fluidRow(),
                title="Build output",
                footer = 
                  tagList(
                    modalButton("Ok")
                  ),
                easyClose = TRUE
              )
            )
          }
        }
      )
      
      ### Output for player traits based on the player Type
      output$playerTraits <- renderUI({
        if(input$playerType == "Player"){
          tags$style(type='text/css', ".selectize-dropdown-content {max-height: 400px; }")
          
          fluidRow(
            column(
              width = 12,
              box(
                title = "Player Traits and Position",
                solidHeader = TRUE,
                width = NULL,
                status = "success",
                
                ##---------------------------------------------------------------
                ##                        Player Traits                         -
                ##---------------------------------------------------------------
                column(
                  width = 5,
                  offset = 1,
                  h5("Select two traits when creating your player. Additional traits, max 5 for 7 total, can be purchased at the Player Store for $3 million each."),
                  selectizeInput(
                    inputId = session$ns("playerTraits"),
                    label = "Select traits for your player",
                    multiple = TRUE,
                    choices = 
                      list(
                        "Movement - On the Ball" = 
                          list(
                            "Cuts Inside From Both Wings",
                            "Knocks Ball Past Opponent",
                            "Runs With Ball Rarely",
                            "Runs With Ball Often",
                            "Runs With Ball Down Left",
                            "Runs With Ball Down Right",
                            "Runs With Ball Through Center",
                            "Stops Play"
                          ),
                        "Movement - Off the Ball" = 
                          list(
                            "Arrives Late In Opponents' Area",
                            "Comes Deep To Get Ball",
                            "Gets Forward Whenever Possible",
                            "Gets Into Opposition Area",
                            "Hugs Line",
                            "Likes to Try to Beat Offside Trap",
                            "Moves Into Channels",
                            "Plays One-Twos",
                            "Plays With Back to Goal",
                            "Does Not Move Into Channels",
                            "Stays Back at All Times"
                          ),
                        "Passing" = 
                          list(
                            "Dictates Tempo",
                            "Likes to Switch Ball to Other Flank",
                            "Looks For Pass Rather Than Attempting to Score",
                            "Plays No Through Balls",
                            "Plays Short Simple Passes",
                            "Tries Killer Balls Often",
                            "Tries Long Range Passes",
                            "Uses Long Throw to Start Counter Attacks"                  
                          ),
                        "Finishing" = 
                          list(
                            "Attempts Overhead Kicks",
                            "Hits Free Kicks With Power",
                            "Likes to Lob Keeper",
                            "Likes to Round Keeper",
                            "Places Shots",
                            "Refrains From Taking Long Shots",
                            "Shoots From Distance",
                            "Shoots With Power",
                            "Tries First Time Shots",
                            "Tries Long Range Free Kicks"
                          ),
                        "Discipline" = 
                          list(
                            "Argues With Officials"
                          ),
                        "Defending" = 
                          list(
                            "Dives Into Tackles",
                            "Does Not Dive Into Tackles",
                            "Marks Opponent Tightly"
                          ),
                        "Technique" =
                          list(
                            "Avoids Using Weaker Foot",
                            "Curls Ball",
                            "Dwells On Ball",
                            "Possesses Long Flat Throw",
                            "Tries to Play Way Out of Trouble"
                          )
                      )
                )
              ),
                ##---------------------------------------------------------------
                ##                        Player Position                       -
                ##---------------------------------------------------------------
                column(
                  width = 5,
                  h5("Distribute 50 experience points among the 
                     different positions your player can play.
                     As per rule 2.2.a.i of the Rulebook, your main
                     position must have 20 XP."),
                  em("Double click on the Experience value you want 
                     to edit. The maximum experience in a position is 
                     20."),
                  DTOutput(outputId = session$ns("posExp"))
                )
            )
          )
        )
      } else {
        NULL
      }
          
    })
      output$posExp <- renderDT({
        reactives$positionalExperience %>% 
          datatable(
            editable = "cell",
            rownames = FALSE,
            selection = "none",
            options = 
              list(
                ordering = FALSE, 
                ## Sets a scroller for the rows
                scrollY = '600px',
                ## Sets size of rows shown
                scrollCollapse = TRUE,
                ## Removes pages in the table
                paging = FALSE,
                ## Adds scrollable horizontal
                scrollX = '600px',
                # pageLength = 10,
                # lengthMenu = c(10, 25, 50, 100),
                dom = 't'
              )
          ) %>% 
          formatStyle(
            columns = "Experience",
            backgroundColor = "white"
          )
      })
      
      posExpProxy <- dataTableProxy(session$ns("posExp"))
      
      observeEvent(
        input$posExp_cell_edit,
        {
          info = input$posExp_cell_edit
          str(info)
          i = info$row
          j = info$col + 1
          v = info$value
          
          reactives$positionalExperience[i,j] <- 
            isolate(
              DT::coerceValue(
                v, 
                reactives$positionalExperience[i, j]
              )
            )
        }
      )
      
      ### Table with the cost of all attribute values
      output$costTPE <- renderDT({
        datatable(
          data.frame(
            `Value` = c("5-7", "8-10", "11-13", "14-16", "17-18", "19-20"),
            Cost = c(2, 4, 6, 12, 18, 25)
          ),
          class = 'compact cell-border stripe',
          rownames = FALSE,
          style = "bootstrap",
          escape = FALSE,
          options = 
            list(
              ordering = FALSE,
              dom = 't',
              ## Sets color of table background
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#00044d', 'color': '#fff'});",
                "}")
            )
        )  
      })
      
      ### TPE information UI
      output$usedTPE <- renderUI({
        tagList(
          fluidRow(
            column(
              width = 6,
              h5("Used TPE")
            ),
            column(
              width = 6,
              h5(currentCost())
            )
          ),
          fluidRow(
            column(
              width = 6,
              h5("Available TPE")
            ),
            column(
              width = 6,
              h5(currentAvailable())
            )
          )
        )
      })
      
      ### Building the player
      {output$costAggression <- renderUI({
        tpeCost$cost[tpeCost$value == input$Aggression]
      })
      
      output$costAnticipation <- renderUI({
        tpeCost$cost[tpeCost$value == input$Anticipation]
      })
      
      output$costBravery <- renderUI({
        tpeCost$cost[tpeCost$value == input$Bravery]
      })
      
      output$costComposure <- renderUI({
        tpeCost$cost[tpeCost$value == input$Composure]
      })
      
      output$costConcentration <- renderUI({
        tpeCost$cost[tpeCost$value == input$Concentration]
      })
      
      output$costDecisions <- renderUI({
        tpeCost$cost[tpeCost$value == input$Decisions]
      })
      
      output$costDetermination <- renderUI({
        tpeCost$cost[tpeCost$value == input$Determination]
      })
      
      output$costFlair <- renderUI({
        tpeCost$cost[tpeCost$value == input$Flair]
      })
      
      output$costLeadership <- renderUI({
        tpeCost$cost[tpeCost$value == input$Leadership]
      })
      
      output$costOffTheBall <- renderUI({
        tpeCost$cost[tpeCost$value == input$OffTheBall]
      })
      
      output$costPositioning <- renderUI({
        tpeCost$cost[tpeCost$value == input$Positioning]
      })
      
      output$costTeamwork <- renderUI({
        tpeCost$cost[tpeCost$value == input$Teamwork]
      })
      
      output$costVision <- renderUI({
        tpeCost$cost[tpeCost$value == input$Vision]
      })
      
      output$costWorkRate <- renderUI({
        tpeCost$cost[tpeCost$value == input$WorkRate]
      })
      
      output$costAcceleration <- renderUI({
        tpeCost$cost[tpeCost$value == input$Acceleration]
      })
      
      output$costAgility <- renderUI({
        tpeCost$cost[tpeCost$value == input$Agility]
      })
      
      output$costBalance <- renderUI({
        tpeCost$cost[tpeCost$value == input$Balance]
      })
      
      output$costJumpingReach <- renderUI({
        tpeCost$cost[tpeCost$value == input$JumpingReach]
      })
      
      output$costNaturalFitness <- renderUI({
        0
      })
      
      output$costPace <- renderUI({
        tpeCost$cost[tpeCost$value == input$Pace]
      })
      
      output$costStamina <- renderUI({
        0
      })
      
      output$costStrength <- renderUI({
        tpeCost$cost[tpeCost$value == input$Strength]
      })
      
      output$costCorners <- renderUI({
        tpeCost$cost[tpeCost$value == input$Corners]
      })
      
      output$costCrossing <- renderUI({
        tpeCost$cost[tpeCost$value == input$Crossing]
      })
      
      output$costDribbling <- renderUI({
        tpeCost$cost[tpeCost$value == input$Dribbling]
      })
      
      output$costFinishing <- renderUI({
        tpeCost$cost[tpeCost$value == input$Finishing]
      })
      
      output$costFirstTouch <- renderUI({
        tpeCost$cost[tpeCost$value == input$FirstTouch]
      })
      
      output$costFreeKick <- renderUI({
        tpeCost$cost[tpeCost$value == input$FreeKick]
      })
      
      output$costHeading <- renderUI({
        tpeCost$cost[tpeCost$value == input$Heading]
      })
      
      output$costLongShots <- renderUI({
        tpeCost$cost[tpeCost$value == input$LongShots]
      })
      
      output$costLongThrows <- renderUI({
        tpeCost$cost[tpeCost$value == input$LongThrows]
      })
      
      output$costMarking <- renderUI({
        tpeCost$cost[tpeCost$value == input$Marking]
      })
      
      output$costPassing <- renderUI({
        tpeCost$cost[tpeCost$value == input$Passing]
      })
      
      output$costPenaltyTaking <- renderUI({
        tpeCost$cost[tpeCost$value == input$PenaltyTaking]
      })
      
      output$costTackling <- renderUI({
        tpeCost$cost[tpeCost$value == input$Tackling]
      })
      
      output$costTechnique <- renderUI({
        tpeCost$cost[tpeCost$value == input$Technique]
      })}
      
      ### Building the goalie
      {output$GoaliecostAggression <- renderUI({
        tpeCost$cost[tpeCost$value == input$GoalieAggression]
      })
        
        output$GoaliecostAnticipation <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieAnticipation]
        })
        
        output$GoaliecostBravery <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieBravery]
        })
        
        output$GoaliecostComposure <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieComposure]
        })
        
        output$GoaliecostConcentration <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieConcentration]
        })
        
        output$GoaliecostDecisions <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieDecisions]
        })
        
        output$GoaliecostDetermination <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieDetermination]
        })
        
        output$GoaliecostFlair <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieFlair]
        })
        
        output$GoaliecostLeadership <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieLeadership]
        })
        
        output$GoaliecostOffTheBall <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieOffTheBall]
        })
        
        output$GoaliecostPositioning <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoaliePositioning]
        })
        
        output$GoaliecostTeamwork <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieTeamwork]
        })
        
        output$GoaliecostVision <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieVision]
        })
        
        output$GoaliecostWorkRate <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieWorkRate]
        })
        
        output$GoaliecostAcceleration <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieAcceleration]
        })
        
        output$GoaliecostAgility <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieAgility]
        })
        
        output$GoaliecostBalance <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieBalance]
        })
        
        output$GoaliecostJumpingReach <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieJumpingReach]
        })
        
        output$GoaliecostNaturalFitness <- renderUI({
          0
        })
        
        output$GoaliecostPace <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoaliePace]
        })
        
        output$GoaliecostStamina <- renderUI({
          0
        })
        
        output$GoaliecostStrength <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieStrength]
        })
        
        output$GoaliecostCorners <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieCorners]
        })
        
        output$GoaliecostCrossing <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieCrossing]
        })
        
        output$GoaliecostDribbling <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieDribbling]
        })
        
        output$GoaliecostFinishing <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieFinishing]
        })
        
        output$GoaliecostFirstTouch <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieFirstTouch]
        })
        
        output$GoaliecostFreeKick <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieFreeKick]
        })
        
        output$GoaliecostHeading <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieHeading]
        })
        
        output$GoaliecostLongShots <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieLongShots]
        })
        
        output$GoaliecostLongThrows <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieLongThrows]
        })
        
        output$GoaliecostMarking <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieMarking]
        })
        
        output$GoaliecostPassing <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoaliePassing]
        })
        
        output$GoaliecostPenaltyTaking <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoaliePenaltyTaking]
        })
        
        output$GoaliecostTackling <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieTackling]
        })
        
        output$GoaliecostTechnique <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieTechnique]
        })
        
        output$GoaliecostAerialReach <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieAerialReach]
        })
        
        output$GoaliecostCommandOfArea <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieCommandOfArea]
        })
        
        output$GoaliecostCommunication <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieCommunication]
        })
        
        output$GoaliecostEccentricity <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieEccentricity]
        })
        output$GoaliecostFirstTouch <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieFirstTouch]
        })
        
        output$GoaliecostHandling <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieHandling]
        })
        
        output$GoaliecostKicking <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieKicking]
        })
        
        output$GoaliecostOneOnOnes <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieOneOnOnes]
        })
        
        output$GoaliecostPassing <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoaliePassing]
        })
        
        output$GoaliecostTendencyToPunch <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieTendencyToPunch]
        })
        output$GoaliecostReflexes <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieReflexes]
        })
        
        output$GoaliecostTendencyToRush <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieTendencyToRush]
        })
        
        output$GoaliecostThrowing <- renderUI({
          tpeCost$cost[tpeCost$value == input$GoalieThrowing]
        })
        
        
        }

      
      
    }
  )
}

