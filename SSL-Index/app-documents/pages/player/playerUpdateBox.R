playerUpdateBoxUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width = 12,
      box(
        title = "Update Player", collapsible = FALSE, width = NULL,
        radioButtons(
          inputId = ns("playerType"),
          label = "Outfield or Goalkeeper",
          choices = c("Outfield", "Goalkeeper"),
          selected = "Outfield"
        ) %>% 
          column(width = 12) %>% 
          fluidRow() %>% 
          div(align = "center", id = ns("playerSelector"))  %>% 
          hidden(),
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
  )
}

playerUpdateBoxServer <- function(id, pid) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$name <- renderUI({
        getPlayerName(pid = pid) %>% 
          then(
            onFulfilled = function(value){
              h2(value$name %>% paste(paste("(", value$class, ")", sep = ""), sep = ", "))
            },
            onRejected = function(error){
              print("something is wrong")
            }
          )
      })
      
      output$traits <- renderUI({
        getPlayerTraits(pid = pid) %>% 
          then(
            onFulfilled = function(value){
              tagList(
                h4("Player Traits"),
                if(value %>% length() == 0){
                  "No traits"
                } else {
                  paste(value, collapse = "<br>") %>% HTML()
                }
              )
            },
            onRejected = function(error){
              print("something is wrong")
            }
          )
      })
      
      output$positions <- renderUI({
        getPlayerPositions(pid = pid) %>% 
          then(
            onFulfilled = function(value){
              tagList(
                h4("Primary Position(s)"),
                value %>% filter(value == 20) %>% select(name) %>% unlist() %>% paste(collapse = ", ") %>% HTML(),
                h4("Secondary Position(s)"),
                value %>% filter(value < 20, value >= 10) %>% select(name)  %>% unlist() %>% paste(collapse = ", ") %>% HTML(),
              )
            },
            onRejected = function(error){
              print("something is wrong")
            }
          )
      })
      
      output$team <- renderImage({
        getPlayerTeam(pid) %>%
          then(
            onFulfilled = function(value){
              list(
                src = normalizePath(file.path("./www", sprintf("%s.png", value$team))),
                width = 100,
                height = 100,
                alt = value$team
              )
            }
          )
      },
      deleteFile = FALSE)
    }
  )
}
