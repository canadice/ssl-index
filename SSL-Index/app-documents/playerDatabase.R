
###########################################################################
###########################################################################
###                                                                     ###
###                         PLAYER BUILDER TOOL                         ###
###                                                                     ###
###########################################################################
###########################################################################

### UI module for player similarities using MDS
playerDatabaseUI <- function(id){
  
  ## Creating the namespacing function for all iDs
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 4,
          selectInput(
            inputId = ns("player"),
            label = "Select a player",
            choices = 
              playerData %>% 
                select(Name) %>% 
                arrange(Name)
            # select(Team, Name) %>% 
            # group_by(Team) %>% 
            # arrange(Name) %>% 
            # group_split(.keep = FALSE) %>% 
            # setNames(playerData$Team %>% unique() %>% sort()) %>% 
            # lapply(
            #   X = .,
            #   FUN = function(x){
            #     c(x) %>% 
            #       unname() %>% 
            #       lapply(
            #         X = .,
            #         FUN = function(x){
            #           as.list(x)
            #         }
            #       ) %>% 
            #       unlist(recursive = FALSE)
            #   }
            # )
          )
        ),
        column(
          width = 8,
          plotlyOutput(
            outputId = ns("radarPlotly"),
            height = "250px"
          )
        )
      ),
      fluidRow(
        tabBox(
          width = NULL,
          
          ##---------------------------------------------------------------
          ##                      Player Updater Tool                     -
          ##---------------------------------------------------------------
          
          tabPanel(
            "Player Updater",
            
            fluidRow(
              ##  Left hand side with information  
              column(
                width = 4,
                box(
                  title = "Information",
                  status = "info",
                  solidHeader = TRUE,
                  width = NULL,
                  h4("TPE Information", align = "center"),
                  fluidRow(
                    column(
                      width = 10,
                      offset = 1,
                      uiOutput(
                        outputId = ns("claimedTPE")
                      ),
                      uiOutput(
                        outputId = ns("earnedTPE")
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
              ##  Right hand side with the builder  
              column(
                width = 8,
                p("Edit the Value column by double clicking on the cell of the attribute you want to edit. 
                Clicking on export provides a formatted text with all the updates you have made on your build."),
                DTOutput(ns("attributeTable"), width = "80%")
              )
            )
          ),
          
          ##----------------------------------------------------------------
          ##                        Game Log Stats                         -
          ##----------------------------------------------------------------
          
          tabPanel(
            "Game Log",
            fluidRow(
              column(
                width = 4,
                selectizeInput(
                  inputId = ns("gameSeason"),
                  label = "Filter Season",
                  choices =
                    playerGameData$Season %>%
                    unique() %>%
                    sort(),
                  multiple = TRUE
                )
              ),
              column(
                width = 4,
                selectizeInput(
                  inputId = ns("gameFilter"),
                  label = "Filter type of Game",
                  choices =
                    c(
                      "Cup",
                      "League"
                    ),
                  multiple = TRUE
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                reactableOutput(
                  outputId = ns("GameData")
                )
              )
            )
          ),
          
          ##----------------------------------------------------------------
          ##                      Career Summary Stats                     -
          ##----------------------------------------------------------------
          
          tabPanel(
            "Career Stats",
            fluidRow(
              column(
                width = 4,
                selectInput(
                  #same as gameFilter, but for career tab
                  inputId = ns("careerFilter"),
                  label = "Filter type of Game",
                  choices =
                    c(
                      "League",
                      "Cup"
                    ),
                  multiple = FALSE
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                reactableOutput(
                  outputId = ns("CareerData")
                )
              )
            )
          ),
          
          ##---------------------------------------------------------------
          ##                      Development History                     -
          ##---------------------------------------------------------------
          
          tabPanel(
            "Development History",
            fluidRow(
              column(
                width = 10,
                offset = 1,
                p("The following plots show the development of the player over their career. 
                  The subplots show the different groups of attributes, and the color codes are
                  unique within each subplot. You can click on the attribute in the legend to hide 
                  the line, or double-click to isolate the line in the plot."),
                plotlyOutput(
                  outputId = ns("Development"),
                  height = "800px"
                )
              )
            )
          )
          
          ##---------------------------------------------------------------
          ##                          End of tabs                         -
          ##---------------------------------------------------------------
          
        )
      )
    )
 )
}

## Backend module for player similarities
playerDatabaseSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){
      
      con <- 
        dbConnect(
          SQLite(), 
          dbFile
        )
      
      ##----------------------------------------------------------------
      ##                Loading data from the Database                 -
      ##----------------------------------------------------------------
      
      careerSeasonData <- 
        reactive({
          
          filterName <- input$player
          
          if(any(reactives$currentBuild$Group == "Goalkeeper")){
            
            table <- 
              tbl(con, "gameDataKeeper") %>% 
              filter(
                Name == filterName
                )
            
            if(input$careerFilter == "Cup"){
              table <- 
                table %>% 
                filter(
                  Matchday %like% "%Cup%"
                )
            } else {
              table <- 
                table %>% 
                filter(
                  !(Matchday %like% "%Cup%")
                )
            }
            
            table %>%  
              group_by(
                Season
              ) %>% 
              summarize(
                Club = Club,
                Apps = 
                  sum(`Apps`, na.rm = TRUE),
                `Minutes Played` = 
                  sum(`Minutes Played`, na.rm = TRUE),
                `Average Rating` = 
                  mean(`Average Rating`, na.rm = TRUE) %>% round(2),
                `Player of the Match` = 
                  sum(`Player of the Match`, na.rm = TRUE),
                Won = 
                  sum(`Won`, na.rm = TRUE),
                Lost = 
                  sum(`Lost`, na.rm = TRUE),
                Drawn = 
                  sum(`Drawn`, na.rm = TRUE),
                `Clean Sheets` = 
                  sum(`Clean Sheets`, na.rm = TRUE),
                Conceded = 
                  sum(`Conceded`, na.rm = TRUE),
                `Saves Parried` = 
                  sum(`Saves Parried`, na.rm = TRUE),
                `Saves Held`= 
                  sum(`Saves Held`, na.rm = TRUE),
                `Saves Tipped` = 
                  sum(`Saves Tipped`, na.rm = TRUE),
                `Save%` = 
                  sum(`Save%`, na.rm = TRUE),
                `Penalties Faced`= 
                  sum(`Penalties Faced`, na.rm = TRUE),
                `Penalties Saved` = 
                  sum(`Penalties Saved`, na.rm = TRUE),
                `xSave%` = 
                  mean(`xSave%`, na.rm = TRUE) %>% round(2)
              ) %>% 
              group_by(
                Season
              ) %>% 
              mutate(
                `Save%` = 
                  ((sum(`Saves Parried`, na.rm = TRUE)+ sum(`Saves Held`, na.rm = TRUE) + sum(`Saves Tipped`, na.rm = TRUE))/
                     (sum(`Saves Parried`, na.rm = TRUE)+ sum(`Saves Held`, na.rm = TRUE) + sum(`Saves Tipped`, na.rm = TRUE) + sum(`Conceded`, na.rm = TRUE))) %>% 
                  round(4)*100
              ) %>% 
              collect()
            
          } else {
            ### Players
            
            table <- 
              tbl(con, "gameDataPlayer") %>% 
              filter(
                Name == filterName
              )
            
            if(input$careerFilter == "Cup"){
              table <- 
                table %>% 
                filter(
                  Matchday %like% "%Cup%"
                )
            } else {
              table <- 
                table %>% 
                filter(
                  !(Matchday %like% "%Cup%")
                )
            }
            
            table %>%  
              group_by(
                Season
              ) %>% 
              summarize(
                Club = Club,
                Apps = sum(Apps),
                `Minutes Played` = 
                  sum(`Minutes Played`),
                `Distance Run (km)` = 
                  sum(`Distance Run (km)`),
                `Average Rating` = 
                  mean(`Average Rating`) %>% round(2),
                `Player of the Match` = 
                  sum(`Player of the Match`),
                Goals = 
                  sum(`Goals`),
                Assists = 
                  sum(`Assists`),
                xG = 
                  sum(`xG`) %>% round(2),
                `Shots on Target` = 
                  sum(`Shots on Target`),
                Shots = 
                  sum(`Shots`),
                `Penalties Taken` = 
                  sum(`Penalties Taken`),
                `Penalties Scored` = 
                  sum(`Penalties Scored`),
                `Successful Passes` = 
                  sum(`Successful Passes`),
                `Attempted Passes` = 
                  sum(`Attempted Passes`),
                `Pass%` = 
                  sum(`Pass%`),
                `Key Passes` = 
                  sum(`Key Passes`),
                `Successful Crosses` = 
                  sum(`Successful Crosses`),
                `Attempted Crosses` = 
                  sum(`Attempted Crosses`),
                `Cross%` = 
                  sum(`Cross%`),
                `Chances Created` = 
                  sum(`Chances Created`),
                `Successful Headers` = 
                  sum(`Successful Headers`),
                `Attempted Headers` = 
                  sum(`Attempted Headers`),
                `Header%` = 
                  sum(`Header%`),
                `Key Headers` = 
                  sum(`Key Headers`),
                Dribbles = 
                  sum(`Dribbles`),
                `Tackles Won` = 
                  sum(`Tackles Won`),
                `Attempted Tackles` = 
                  sum(`Attempted Tackles`),
                `Tackle%` =
                  sum(`Tackle%`),
                `Key Tackles`= 
                  sum(`Key Tackles`),
                Interceptions = 
                  sum(`Interceptions`),
                Clearances = 
                  sum(`Clearances`),
                `Mistakes Leading to Goals` = 
                  sum(`Mistakes Leading to Goals`),
                `Yellow Cards` = 
                  sum(`Yellow Cards`),
                `Red Cards` = 
                  sum(`Red Cards`),
                Fouls = 
                  sum(`Fouls`),
                `Fouls Against` = 
                  sum(`Fouls Against`),
                Offsides = 
                  sum(`Offsides`)
              ) %>%
              group_by(
                Season
              ) %>% 
              mutate(
                `Pass%` = 
                  round(sum(`Successful Passes`)/sum(`Attempted Passes`), 4)*100,
                `Cross%` = 
                  round(sum(`Successful Crosses`)/sum(`Attempted Crosses`), 4)*100,
                `Header%` = 
                  round(sum(`Successful Headers`)/sum(`Attempted Headers`), 4)*100,
                `Tackle%` =
                  round(sum(`Tackles Won`)/sum(`Attempted Tackles`), 4)*100
              ) %>% 
              collect()
          }
        })
      
      careerData <- 
        reactive({
          
          filterName <- input$player
          
          if(any(reactives$currentBuild$Group == "Goalkeeper")){
            table <- 
              tbl(con, "gameDataKeeper") %>% 
              filter(
                Name == filterName
              )
            
            if(input$careerFilter == "Cup"){
              table <- 
                table %>% 
                filter(
                  Matchday %like% "%Cup%"
                )
            } else {
              table <- 
                table %>% 
                filter(
                  !(Matchday %like% "%Cup%")
                )
            }
            
            table %>%
              # group_by(
              #   Season
              # ) %>% 
              summarize(
                Club = Club,
                Apps = 
                  sum(`Apps`, na.rm = TRUE),
                `Minutes Played` = 
                  sum(`Minutes Played`, na.rm = TRUE),
                `Average Rating` = 
                  mean(`Average Rating`, na.rm = TRUE) %>% round(2),
                `Player of the Match` = 
                  sum(`Player of the Match`, na.rm = TRUE),
                Won = 
                  sum(`Won`, na.rm = TRUE),
                Lost = 
                  sum(`Lost`, na.rm = TRUE),
                Drawn = 
                  sum(`Drawn`, na.rm = TRUE),
                `Clean Sheets` = 
                  sum(`Clean Sheets`, na.rm = TRUE),
                Conceded = 
                  sum(`Conceded`, na.rm = TRUE),
                `Saves Parried` = 
                  sum(`Saves Parried`, na.rm = TRUE),
                `Saves Held`= 
                  sum(`Saves Held`, na.rm = TRUE),
                `Saves Tipped` = 
                  sum(`Saves Tipped`, na.rm = TRUE),
                `Save%` = 
                  sum(`Save%`, na.rm = TRUE),
                `Penalties Faced`= 
                  sum(`Penalties Faced`, na.rm = TRUE),
                `Penalties Saved` = 
                  sum(`Penalties Saved`, na.rm = TRUE),
                `xSave%` = 
                  mean(`xSave%`, na.rm = TRUE) %>% round(2)
              ) %>% 
              mutate(
                `Save%` = 
                  ((sum(`Saves Parried`, na.rm = TRUE)+ sum(`Saves Held`, na.rm = TRUE) + sum(`Saves Tipped`, na.rm = TRUE))/
                     (sum(`Saves Parried`, na.rm = TRUE)+ sum(`Saves Held`, na.rm = TRUE) + sum(`Saves Tipped`, na.rm = TRUE) + sum(`Conceded`, na.rm = TRUE))) %>% 
                  round(4)*100
              ) %>% 
              collect()
          } else {
            table <- 
              tbl(con, "gameDataPlayer") %>% 
              filter(
                Name == filterName
              )
            
            if(input$careerFilter == "Cup"){
              table <- 
                table %>% 
                filter(
                  Matchday %like% "%Cup%"
                )
            } else {
              table <- 
                table %>% 
                filter(
                  !(Matchday %like% "%Cup%")
                )
            }
            
            table %>% 
              # group_by(
              #   Season
              # ) %>% 
              summarize(
                Club = Club,
                Apps = sum(Apps),
                `Minutes Played` = 
                  sum(`Minutes Played`),
                `Distance Run (km)` = 
                  sum(`Distance Run (km)`),
                `Average Rating` = 
                  mean(`Average Rating`) %>% round(2),
                `Player of the Match` = 
                  sum(`Player of the Match`),
                Goals = 
                  sum(`Goals`),
                Assists = 
                  sum(`Assists`),
                xG = 
                  sum(`xG`) %>% round(2),
                `Shots on Target` = 
                  sum(`Shots on Target`),
                Shots = 
                  sum(`Shots`),
                `Penalties Taken` = 
                  sum(`Penalties Taken`),
                `Penalties Scored` = 
                  sum(`Penalties Scored`),
                `Successful Passes` = 
                  sum(`Successful Passes`),
                `Attempted Passes` = 
                  sum(`Attempted Passes`),
                `Pass%` = 
                  sum(`Pass%`),
                `Key Passes` = 
                  sum(`Key Passes`),
                `Successful Crosses` = 
                  sum(`Successful Crosses`),
                `Attempted Crosses` = 
                  sum(`Attempted Crosses`),
                `Cross%` = 
                  sum(`Cross%`),
                `Chances Created` = 
                  sum(`Chances Created`),
                `Successful Headers` = 
                  sum(`Successful Headers`),
                `Attempted Headers` = 
                  sum(`Attempted Headers`),
                `Header%` = 
                  sum(`Header%`),
                `Key Headers` = 
                  sum(`Key Headers`),
                Dribbles = 
                  sum(`Dribbles`),
                `Tackles Won` = 
                  sum(`Tackles Won`),
                `Attempted Tackles` = 
                  sum(`Attempted Tackles`),
                `Tackle%` =
                  sum(`Tackle%`),
                `Key Tackles`= 
                  sum(`Key Tackles`),
                Interceptions = 
                  sum(`Interceptions`),
                Clearances = 
                  sum(`Clearances`),
                `Mistakes Leading to Goals` = 
                  sum(`Mistakes Leading to Goals`),
                `Yellow Cards` = 
                  sum(`Yellow Cards`),
                `Red Cards` = 
                  sum(`Red Cards`),
                Fouls = 
                  sum(`Fouls`),
                `Fouls Against` = 
                  sum(`Fouls Against`),
                Offsides = 
                  sum(`Offsides`)
              ) %>%
              mutate(
                `Pass%` = 
                  round(sum(`Successful Passes`)/sum(`Attempted Passes`), 4)*100,
                `Cross%` = 
                  round(sum(`Successful Crosses`)/sum(`Attempted Crosses`), 4)*100,
                `Header%` = 
                  round(sum(`Successful Headers`)/sum(`Attempted Headers`), 4)*100,
                `Tackle%` =
                  round(sum(`Tackles Won`)/sum(`Attempted Tackles`), 4)*100
              ) %>% 
              collect()
          }
          # MatchDay like \"%", input$gameFilter, "%\" AND
        })
      
      ##---------------------------------------------------------------
      ##                        Reactive values                       -
      ##---------------------------------------------------------------
      
      currentAvailable <- reactiveVal(350)
      
      currentCost <- reactiveVal(0)
      
      radarAttributes <- reactive(
        playerData %>% 
          filter(Name == input$player) %>% 
          select(
            Name,
            DEFENDING:MENTAL
          )
      )
      
      ## Reactive data set that updates the input build of a player
      reactives <- reactiveValues(
        currentBuild = {
        playerData %>% 
          filter(Name == input$player) %>% 
          select(
            Acceleration:Throwing
          ) %>% 
          pivot_longer(
            cols = everything(),
            names_to = "Attribute",
            values_to = "Value"
          ) %>% 
          left_join(
            tpeCost,
            by = c("Value" = "value")
          ) %>% 
          mutate(
            cost = if_else(Attribute %in% c("Natural Fitness", "Stamina"), 0, cost)
          ) %>% 
          left_join(
            attributes,
            by = "Attribute"
          ) %>% 
          filter(
            !is.na(Value)
          ) %>% 
          select(
            -Keeper,
            -abbr
          )
      })
      
      ##---------------------------------------------------------------
      ##                          Observers                           -
      ##---------------------------------------------------------------
      
      ## Observer that calculates the correct used TPE and available TPE for the build
      observeEvent(
        input$earnedTPE,
        {
          currentAvailable(
            (playerData %>% filter(Name == input$player) %>% select(TPE) %>% unlist() %>% unname()) +
              input$earnedTPE -
              sum(reactives$currentBuild$cost)
          )
          currentCost(sum(reactives$currentBuild$cost))
        },
        ignoreInit = FALSE
      )
      
      ## Observes a change in player and updates the current build and costs accordingly
      observeEvent(
        input$player,
        {
          reactives$currentBuild <- 
            playerData %>% 
            filter(Name == input$player) %>% 
            select(
              Acceleration:Throwing
            ) %>% 
            pivot_longer(
              cols = everything(),
              names_to = "Attribute",
              values_to = "Value"
            ) %>% 
            left_join(
              tpeCost,
              by = c("Value" = "value")
            ) %>% 
            mutate(
              cost = if_else(Attribute %in% c("Natural Fitness", "Stamina"), 0, cost)
            ) %>% 
            left_join(
              attributes,
              by = "Attribute"
            ) %>% 
            filter(
              !is.na(Value)
            ) %>% 
            select(
              -Keeper
            )
          
          currentAvailable(
            (playerData %>% filter(Name == input$player) %>% select(TPE) %>% unlist() %>% unname()) +
              input$earnedTPE -
              sum(reactives$currentBuild$cost)
            )
          currentCost(sum(reactives$currentBuild$cost))
        }
      )
      
      observeEvent(
        input$attributeTable_cell_edit,
        {
          editedCell = input$attributeTable_cell_edit
          i = as.numeric(editedCell$row)
          j = as.numeric(editedCell$col)
          k = as.numeric(editedCell$value)
          
          # Checks if new value falls within attribute constraints
          if(k < 5){ 
            k <- 5
          } else if(k > 20) {
            k <- 20
          }
          
          # Checks if you alter the Natural Fitness or Stamina which are fixed attributes
          if(i %in% c(5,7)){
            k <- 20 
            
            reactives$currentBuild[i,j] <- k
          } else {
            #write values to reactive
            reactives$currentBuild[i,j] <- k
            
            reactives$currentBuild[i,"cost"] <- tpeCost$cost[tpeCost$value == k]  
          }
          
          currentAvailable(
            (playerData %>% filter(Name == input$player) %>% select(TPE) %>% unlist() %>% unname()) +
              input$earnedTPE -
              sum(reactives$currentBuild$cost)
          )
          currentCost(sum(reactives$currentBuild$cost))
        }
      )
      
      
      ## Observer for the export button that creates a formatted text with all info of the build
      observeEvent(
        input$exportButton,
        {
          current <- 
            playerData %>% 
            filter(Name == input$player) %>% 
            select(
              Acceleration:Throwing
            ) %>% 
            pivot_longer(
              cols = everything(),
              names_to = "Attribute",
              values_to = "Value"
            ) %>% 
            left_join(
              tpeCost,
              by = c("Value" = "value")
            ) %>% 
            mutate(
              cost = if_else(Attribute %in% c("Natural Fitness", "Stamina"), 0, cost)
            ) %>% 
            left_join(
              attributes,
              by = "Attribute"
            ) %>% 
            filter(
              !is.na(Value)
            ) %>% 
            select(
              -Keeper
            )
          
          updated <- 
            reactives$currentBuild
          
          merged <- 
            current %>% 
            left_join(
              updated,
              by = "Attribute",
              suffix = c(".old", ".new")
            ) %>% 
            filter(
              Value.old != Value.new
            ) %>% 
            mutate(
              ChangeCost = cost.new - cost.old
            ) %>% 
            select(
              Attribute,
              Value.old,
              Value.new,
              ChangeCost
            )  
          
          if(merged %>% nrow() > 0){
            showModal(
              modalDialog(
                span(
                  "Copy the code below containing your updates into your update post."
                ),
                br(),
                br(),
                column(
                  width = 8,
                  offset = 2,
                  helpText(
                    paste(
                      paste(
                        paste(
                          "[b]Earned TPE:[/b] ", 
                          playerData %>% filter(Name == input$player) %>% select(TPE) %>% unlist() %>% unname(),
                          "+",
                          input$earnedTPE,
                          "=",
                          (playerData %>% filter(Name == input$player) %>% select(TPE) %>% unlist() %>% unname())+input$earnedTPE
                          ),
                        paste("[b]Used TPE:[/b] ", currentCost()), 
                        paste("[b]Banked TPE:[/b] ", currentAvailable()),
                        sep = "<br>"
                      ),
                      " ",
                      paste(
                        apply(
                          X = merged,
                          MARGIN = 1, 
                          FUN = function(merged){
                            paste(merged[1], paste(merged[2:3], collapse = " -> "), paste("(",merged[4], ")", sep = ""), collapse = " ")    
                          }
                        ),
                        collapse = "<br>"
                      ),
                      sep = "<br>"
                    ) %>% 
                      HTML()
                  ) %>% 
                    div(
                      style = "background: #f0f0f0; border: #656565"
                    )
                ),
                title="Update output",
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
                  "You have not changed your build yet."
                ),
                title="Update output",
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
      
      ##---------------------------------------------------------------
      ##                      Datatable outputs                       -
      ##---------------------------------------------------------------
      
      ## Table with the cost of all attribute values
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
      
      ## TPE information UI
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
      
      output$claimedTPE <- renderUI({
        tagList(
          fluidRow(
            column(
              width = 6,
              h5("Claimed TPE")
            ),
            column(
              width = 6,
              h5(playerData %>% filter(Name == input$player) %>% select(TPE) %>% unlist() %>% unname())
            )
          )
        )
      })
      
      output$earnedTPE <- renderUI({
        tagList(
          fluidRow(
            column(
              width = 6,
              h5("Earned TPE")
            ),
            column(
              width = 6,
              numericInput(
                inputId = session$ns("earnedTPE"),
                label = NULL,
                value = 0,
                min = 0,
                max = 500,
                width = "80%"
              )
            )
          )
        )
      })
      
      ##----------------------------------------------------------------
      ##              Datatable output for attributes                  -
      ##----------------------------------------------------------------
      
      output$attributeTable <- renderDT({
        datatable(
          reactives$currentBuild %>% 
            relocate(
              Group,
              .before = Attribute
            ) %>% 
            rename(
              Cost = cost
            ) %>% 
            select(
              -abbr
            ), 
          rownames = FALSE,
          selection = "none",
          editable = 
            list(
              target = "cell",
              disable = 
                list(
                  columns = c(0,1,3)
                )
            ),
          extensions = "RowGroup",
          options = 
            list(
              rowGroup = list(datSrc = 0),
              ordering = FALSE, 
              pageLength = -1,
              dom = 'Rt',
              ## Sets color of table background
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#00044d', 'color': '#fff'});",
                "}")
            )
          )
      })
      
      # Using reactable for Game Log
      ##---------------------------------------------------------------
      ##                        Game log output                       -
      ##---------------------------------------------------------------

      output$GameData <- renderReactable({
        filterName <- input$player
        
        if(any(reactives$currentBuild$Group == "Goalkeeper")){
          data <- 
            tbl(con, "gameDataKeeper") %>% 
            filter(
              Name == filterName
            ) %>% 
            collect()
         
        } else {
          data <- 
            tbl(con, "gameDataPlayer") %>% 
            filter(
              Name == filterName
            ) %>% 
            select(
              -(Acc:Wor)  
            ) %>% 
            collect()
            
        }
        
        if(!is.null(input$gameSeason)){
          data <- 
            data %>% 
            filter(
              Season %in% input$gameSeason
            )
        }
        
        if(!is.null(input$gameFilter)){
          if(input$gameFilter %>% length() == 2){
            # Do nothing
          } else if("Cup" %in% input$gameFilter){
            data <- 
              data %>% 
              filter(
                str_detect(Matchday, "Cup")
              )
          } else {
            data <- 
              data %>% 
              filter(
                str_detect(Matchday, "Cup", negate = TRUE)
              )
          }
        }
        
        data %>% 
          select(
            -Name,
            -Nationality,
            -Position,
            -Apps
          ) %>%
          relocate(
            c(
              Season,
              Division,
              Matchday,
              Club,
              Opponent,
              Result
            ),
            .before = `Minutes Played` 
          ) %>% 
          mutate(
            Division = as.character(Division)
          ) %>% 
          mutate(
            Division = if_else(Division == "0", "Cup", Division)
          ) %>% 
          reactable(
            pagination = FALSE,
            rowStyle = function(index) {
              if (data$Season[index] %% 2 == 0) list(background = "rgba(0, 0, 0, 0.05)")
              else list(background = "rgba(0, 0, 0, 0.0)")
            },
            columns = 
              list(
                Club = 
                  colDef(
                    maxWidth = 50,
                    align = "center",
                    class = "cell",
                    cell = function(value){
                      logo <- 
                        img(
                          class = "logo",
                          src = teamInfo$logo[teamInfo$team == value],
                          alt = value,
                          height = 30
                        )
                      
                      div(class = "club", logo)
                    }
                  ),
                Opponent = 
                  colDef(
                    name = "Opp.",
                    maxWidth = 50,
                    class = "cell",
                    align = "center",
                    cell = function(value){
                      
                      if(value %>% is.na()){
                        div(class = "club", " ")
                        
                      } else {
                        logo <- 
                          img(
                            class = "logo",
                            src = teamInfo$logo[teamInfo$team == value],
                            alt = value,
                            height = 30
                          )
                        div(class = "club", logo)
                      }
                    }
                  ),
                Result = 
                  colDef(
                    maxWidth = 60,
                    cell = function(value) {
                      if(is.na(value)){
                        div(
                          style = list(background = "#FFFFFF"),
                          " "
                        )
                      } else {
                        values <- str_split(value, pattern = "-", simplify = TRUE)
                        
                        if(any(str_detect(values, pattern = "p|e"))){
                          
                          if(which(str_detect(values, pattern = "p|e")) == 1){
                            color <- "#A4D1A2"
                          } else {
                            color <- "#CB8491"
                          }
                          
                        } else {
                          values <- values %>% as.numeric()
                          
                          if(values[1] > values[2]){
                            color <- "#A4D1A2"
                          } else if(values[1] < values[2]){
                            color <- "#CB8491"
                          } else {
                            color <- "#FFFFBF"
                          }
                        }
                        
                        div(
                          align = "center",
                          style = list(background = color),
                          value
                        )
                      }
                    }
                  )
              )
          ) 
        
      })
      
      
      # Using CareerData 
      output$CareerData <- renderReactable({
        if(!is.null(input$careerFilter)){
          data <- 
            careerSeasonData()
          
          dataSum <- 
            careerData()
          
          if(any(reactives$currentBuild$Group == "Goalkeeper")){
            data %>% 
              reactable(
                pagination = FALSE,
                columns = 
                  list(
                    Season = colDef(
                      footer = "Totals",
                      maxWidth = 80,
                      style = list(position = "sticky", left = 0, background = "#FFFFFF", zIndex = 1),
                      headerStyle = list(position = "sticky", left = 0, background = "#FFFFFF", zIndex = 1),
                      footerStyle = list(fontWeight = "bold", position = "sticky", left = 0, background = "#FFFFFF", zIndex = 1)
                    ),
                    Club = 
                      colDef(
                        maxWidth = 50,
                        align = "center",
                        class = "cell",
                        cell = function(value, index){
                          logo <- 
                            img(
                              class = "logo",
                              src = teamInfo$logo[teamInfo$team == value],
                              alt = value,
                              height = 30
                            )
                          
                          div(class = "club", logo)
                        }
                      ),
                    `Minutes Played` = colDef(footer = dataSum$`Minutes Played`),
                    `Average Rating` = colDef(footer = dataSum$`Average Rating`),
                    `Player of the Match` = colDef(footer = dataSum$`Player of the Match`),
                    `Won` = colDef(footer = dataSum$`Won`),
                    `Lost` = colDef(footer = dataSum$`Lost`),
                    `Drawn` = colDef(footer = dataSum$`Drawn`),
                    `Clean Sheets` = colDef(footer = dataSum$`Clean Sheets`),
                    `Conceded` = colDef(footer = dataSum$`Conceded`),
                    `Saves Parried` = colDef(footer = dataSum$`Saves Parried`),
                    `Saves Held` = colDef(footer = dataSum$`Saves Held`),
                    `Saves Tipped` = colDef(footer = dataSum$`Saves Tipped`),
                    `Save%` = colDef(footer = dataSum$`Save%`),
                    `Penalties Faced` = colDef(footer = dataSum$`Penalties Faced`),
                    `Penalties Saved` = colDef(footer = dataSum$`Penalties Saved`),
                    `xSave%` = colDef(footer = dataSum$`xSave%`),
                    `Apps` = colDef(footer = dataSum$`Apps`)
                    
                  ),
                defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
              )
              
          } else {
            data %>% 
              reactable(
                pagination = FALSE,
                columns = 
                  list(
                    Season = colDef(
                      footer = "Totals",
                      maxWidth = 80,
                      style = list(position = "sticky", left = 0, background = "#FFFFFF", zIndex = 1),
                      headerStyle = list(position = "sticky", left = 0, background = "#FFFFFF", zIndex = 1),
                      footerStyle = list(fontWeight = "bold", position = "sticky", left = 0, background = "#FFFFFF", zIndex = 1)
                    ),
                    Club = 
                      colDef(
                        maxWidth = 50,
                        align = "center",
                        class = "cell",
                        cell = function(value, index){
                          logo <- 
                            img(
                              class = "logo",
                              src = teamInfo$logo[teamInfo$team == value],
                              alt = value,
                              height = 30
                            )
                          
                          div(class = "club", logo)
                        }
                      ),
                    `Minutes Played` = colDef(footer = dataSum$`Minutes Played`),
                    `Distance Run (km)` = colDef(footer = dataSum$`Distance Run (km)`),
                    `Average Rating` = colDef(footer = dataSum$`Average Rating`),
                    `Player of the Match` = colDef(footer = dataSum$`Player of the Match`),
                    `Goals` = colDef(footer = dataSum$`Goals`),
                    `Assists` = colDef(footer = dataSum$`Assists`),
                    `xG` = colDef(footer = dataSum$`xG`),
                    `Shots on Target` = colDef(footer = dataSum$`Shots on Target`),
                    `Shots` = colDef(footer = dataSum$`Shots`),
                    `Penalties Taken` = colDef(footer = dataSum$`Penalties Taken`),
                    `Penalties Scored` = colDef(footer = dataSum$`Penalties Scored`),
                    `Successful Passes` = colDef(footer = dataSum$`Successful Passes`),
                    `Attempted Passes` = colDef(footer = dataSum$`Attempted Passes`),
                    `Pass%` = colDef(footer = dataSum$`Pass%`),
                    `Key Passes` = colDef(footer = dataSum$`Key Passes`),
                    `Successful Crosses` = colDef(footer = dataSum$`Successful Crosses`),
                    `Attempted Crosses` = colDef(footer = dataSum$`Attempted Crosses`),
                    `Cross%` = colDef(footer = dataSum$`Cross%`),
                    `Chances Created` = colDef(footer = dataSum$`Chances Created`),
                    `Successful Headers` = colDef(footer = dataSum$`Successful Headers`),
                    `Attempted Headers` = colDef(footer = dataSum$`Attempted Headers`),
                    `Header%` = colDef(footer = dataSum$`Header%`),
                    `Key Headers` = colDef(footer = dataSum$`Key Headers`),
                    `Dribbles` = colDef(footer = dataSum$`Dribbles`),
                    `Tackles Won` = colDef(footer = dataSum$`Tackles Won`),
                    `Attempted Tackles` = colDef(footer = dataSum$`Attempted Tackles`),
                    `Tackle%` = colDef(footer = dataSum$`Tackle%`),
                    `Key Tackles` = colDef(footer = dataSum$`Key Tackles`),
                    `Interceptions` = colDef(footer = dataSum$`Interceptions`),
                    `Clearances` = colDef(footer = dataSum$`Clearances`),
                    `Mistakes Leading to Goals` = colDef(footer = dataSum$`Mistakes Leading to Goals`),
                    `Yellow Cards` = colDef(footer = dataSum$`Yellow Cards`),
                    `Red Cards` = colDef(footer = dataSum$`Red Cards`),
                    `Fouls` = colDef(footer = dataSum$`Fouls`),
                    `Fouls Against` = colDef(footer = dataSum$`Fouls Against`),
                    `Offsides` = colDef(footer = dataSum$`Offsides`),
                    `Apps` = colDef(footer = dataSum$`Apps`)
                    
                  ),
                defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
              )
          }
        }
      })
      
      # Using DT
      # output$GameData <- renderDT({
      #   if(any(reactives$currentBuild$Group == "Goalkeeper")){
      #     data <- 
      #       keeperGameData %>% 
      #       filter(
      #         Name == input$player
      #       )
      #   } else {
      #     data <- 
      #       playerGameData %>% 
      #       filter(
      #         Name == input$player
      #       ) 
      #   }
      #   
      #   if(!is.null(input$gameSeason)){
      #     data <- 
      #       data %>% 
      #       filter(
      #         Season %in% input$gameSeason
      #       )
      #   }
      #   
      #   if(!is.null(input$gameFilter)){
      #     if("Cup" %in% input$gameFilter){
      #       data <- 
      #         data %>% 
      #         filter(
      #           str_detect(Matchday, "Cup")
      #         )
      #     } else {
      #       data <- 
      #         data %>% 
      #         filter(
      #           str_detect(Matchday, "Cup", negate = TRUE)
      #         )
      #     }
      #   }
      #   
      #   data %>% 
      #     select(
      #       -Name,
      #       -Nationality,
      #       -Position,
      #       -Apps
      #     ) %>% 
      #     relocate(
      #       Season,
      #       .before = "Minutes Played"
      #     ) %>% 
      #     relocate(
      #       Matchday,
      #       .after = "Season"
      #     ) %>% 
      #     relocate(
      #       c(
      #         Club,
      #         Opponent,
      #         Result
      #       ),
      #       .after = "Matchday"
      #     ) %>% 
      #     mutate(
      #       `Average Rating` = `Average Rating` %>% round(2),
      #       xG = xG %>% round(2)
      #     ) %>% 
      #     rename(
      #       S = Season,
      #       MD = Matchday
      #     )
      # },
      # rownames = FALSE,
      # escape = FALSE,
      # style = "bootstrap",
      # class = 'compact cell-border stripe',
      # selection = "none",
      # extensions = "FixedColumns",
      # options = 
      #   list(
      #     orderClasses = FALSE, 
      #     # order = list(list(0, 'asc')),
      #     ## Sets a scroller for the rows
      #     scrollY = '500px',
      #     ## Sets size of rows shown
      #     scrollCollapse = TRUE,
      #     ## Removes pages in the table
      #     paging = FALSE,
      #     ## Adds scrollable horizontal
      #     scrollX = TRUE,
      #     dom = 't',
      #     bInfo = FALSE,
      #     fixedColumns = 
      #       list(
      #         leftColumns = 4
      #       )
      #   ))
      
      ##----------------------------------------------------------------
      ##                        Visualizations                         -
      ##----------------------------------------------------------------
      
      output$Development <- renderPlotly({
        filterName <- input$player
        
        data <- 
          tbl(con, "gameDataPlayer") %>% 
          filter(
            Name == filterName
          ) %>% 
          select(
            Name,
            Club,
            Result:Division,
            Acc:Wor
          ) %>% 
          collect() %>% 
          mutate(
            index = 1:n()
          ) %>% 
          pivot_longer(
            cols = Acc:Wor
          ) %>% 
          left_join(
            attributes, 
            by = c("name"= "abbr")
          ) %>% 
          filter(
            !(Attribute %in% c("Stamina", "Natural Fitness"))
          )
        
        if(any(reactives$currentBuild$Group == "Goalkeeper")){
          data <- 
            data %>% 
            filter(
              Keeper == TRUE
            )
        } else {
          data <- 
            data %>% 
            filter(
              Group != "Goalkeeper"
            )
        }
        
        
        fig1 <-
          plot_ly(
            data = data %>% 
              filter(Group == "Physical"),
            x = ~index,
            y = ~value,
            color = ~Attribute,
            text = ~Attribute,
            type = "scatter",
            mode = "lines",
            hoverinfo = "text+y",
            colors = "Paired",
            name = 
              paste(
                "Physical",
                data %>%
                  filter(Group == "Physical") %>% 
                  select(Attribute) %>% 
                  unlist(),
                sep = " - "
                )
          ) %>% 
          layout(
            yaxis = 
              list(
                title = "Value",
                range = c(4, 21),
                linecolor = "black",
                linewidth = 0.2,
                mirror = TRUE
              ),
            xaxis = 
              list(
                title = "Career",
                showticklabels = FALSE,
                linecolor = "black",
                linewidth = 0.2,
                mirror = TRUE
              ),
            hovermode = "x unified"
          )
        
        fig2 <- 
          plot_ly(
            data = data %>% 
              filter(Group == "Technical"),
            x = ~index,
            y = ~value,
            color = ~Attribute,
            text = ~Attribute,
            type = "scatter",
            mode = "lines",
            hoverinfo = "text+y",
            colors = "Paired",
            name = 
              paste(
                "Technical",
                data %>%
                  filter(Group == "Technical") %>% 
                  select(Attribute) %>% 
                  unlist(),
                sep = " - "
              )
          ) %>% 
          layout(
            yaxis = 
              list(
                title = "Value",
                range = c(4, 21),
                linecolor = "black",
                linewidth = 0.2,
                mirror = TRUE
              ),
            xaxis = 
              list(
                title = "Career",
                showticklabels = FALSE,
                linecolor = "black",
                linewidth = 0.2,
                mirror = TRUE
              ),
            hovermode = "x unified"
          )
        
        fig3 <- 
          plot_ly(
            data = data %>% 
              filter(Group == "Mental"),
            x = ~index,
            y = ~value,
            color = ~Attribute,
            text = ~Attribute,
            type = "scatter",
            mode = "lines",
            hoverinfo = "text+y",
            colors = "Paired",
            name = 
              paste(
                "Mental",
                data %>%
                  filter(Group == "Mental") %>% 
                  select(Attribute) %>% 
                  unlist(),
                sep = " - "
              )
          ) %>% 
          layout(
            yaxis = 
              list(
                title = "Value",
                range = c(4, 21),
                linecolor = "black",
                linewidth = 0.2,
                mirror = TRUE
              ),
            xaxis = 
              list(
                title = "Career",
                showticklabels = FALSE,
                linecolor = "black",
                linewidth = 0.2,
                mirror = TRUE
              ),
            hovermode = "x unified"
          )
        
        
        if(any(reactives$currentBuild$Group == "Goalkeeper")){
          fig4 <- 
            plot_ly(
              data = data %>% 
                filter(Group == "Goalkeeper"),
              x = ~index,
              y = ~value,
              color = ~Attribute,
              text = ~Attribute,
              type = "scatter",
              mode = "lines",
              hoverinfo = "text+y",
              colors = "Paired",
              name = 
                paste(
                  "Goalkeeper",
                  data %>%
                    filter(Group == "Goalkeeper") %>% 
                    select(Attribute) %>% 
                    unlist(),
                  sep = " - "
                )
            ) %>% 
            layout(
              yaxis = 
                list(
                  title = "Value",
                  range = c(4, 21),
                  linecolor = "black",
                  linewidth = 0.2,
                  mirror = TRUE
                ),
              xaxis = 
                list(
                  title = "Career",
                  showticklabels = FALSE,
                  linecolor = "black",
                  linewidth = 0.2,
                  mirror = TRUE
                ),
              hovermode = "x unified"
            )
          
          fig <- 
            subplot(
              fig1, fig2, fig3, fig4, 
              nrows = 4, 
              shareY = TRUE,
              titleY = FALSE
            ) %>% 
            suppressWarnings()
          
          annotations <-
            list( 
              list( 
                x = -0.1,  
                y = 1.0, 
                font = list(size = 15),
                text = "Physical",  
                xref = "paper",  
                yref = "paper",  
                xanchor = "center",  
                yanchor = "bottom",  
                showarrow = FALSE 
              ),  
              list( 
                x = -0.1,  
                y = 0.72,  
                text = "Technical",  
                font = list(size = 15),
                xref = "paper",  
                yref = "paper",  
                xanchor = "center",  
                yanchor = "bottom",  
                showarrow = FALSE 
              ),  
              list( 
                x = -0.1,  
                y = 0.47,  
                text = "Mental",  
                font = list(size = 15),
                xref = "paper",  
                yref = "paper",  
                xanchor = "center",  
                yanchor = "bottom",  
                showarrow = FALSE 
              ),
              list( 
                x = -0.1,  
                y = 0.22,  
                text = "Goalkeeper",
                font = list(size = 15),
                xref = "paper",  
                yref = "paper",  
                xanchor = "center",  
                yanchor = "bottom",  
                showarrow = FALSE 
              ),
              list( 
                x = 0.5,  
                y = -0.05,  
                text = "Career",  
                font = list(size = 12),
                xref = "paper",  
                yref = "paper",  
                xanchor = "center",  
                yanchor = "bottom",  
                showarrow = FALSE 
              )
            )
          
          
        } else {
          fig <- 
            subplot(
              fig1, fig2, fig3, 
              nrows = 3, 
              shareY = TRUE,
              titleY = FALSE
            ) %>% 
            suppressWarnings()
          
          annotations <-
            list( 
              list( 
                x = -0.1,  
                y = 1.0,  
                text = "Physical",  
                font = list(size = 15),
                xref = "paper",  
                yref = "paper",  
                xanchor = "center",  
                yanchor = "bottom",  
                showarrow = FALSE 
              ),  
              list( 
                x = -0.1,  
                y = 0.63,  
                text = "Technical", 
                font = list(size = 15),
                xref = "paper",  
                yref = "paper",  
                xanchor = "center",  
                yanchor = "bottom",  
                showarrow = FALSE 
              ),  
              list( 
                x = -0.1,  
                y = 0.30,  
                text = "Mental",  
                font = list(size = 15),
                xref = "paper",  
                yref = "paper",  
                xanchor = "center",  
                yanchor = "bottom",  
                showarrow = FALSE 
              ),
              list( 
                x = 0.5,  
                y = -0.05,  
                text = "Career",  
                font = list(size = 12),
                xref = "paper",  
                yref = "paper",  
                xanchor = "center",  
                yanchor = "bottom",  
                showarrow = FALSE 
              )
            )
        }
        
        
        fig %>%
          plotly::config(
            modeBarButtonsToRemove =
              c("zoom", "pan2d", "zoomIn2d", "zoomOut2d", "autoScale2d",
                "resetScale2d", "hoverClosestCartesian",
                "hoverCompareCartesian", "toggleSpikelines"
              )
          ) %>%
          layout(
            annotations = annotations,
            margin = 
              list(
                l = 120
              ),
            # autosize = TRUE,
            dragmode= FALSE,
            ## Legend is put to false so the plot is the same size
            paper_bgcolor='#ffffff00',
            plot_bgcolor='#ffffff00'
          )
        
        
        # p <- ggplot(data) + aes(x = index, y = value, color = Attribute) + geom_line() +
        #   facet_wrap(vars(Group)) + scale_y_continuous(limits = c(5, 20)) +
        #   theme_bw()
        # p %>% 
        #   ggplotly()
      })
      
      output$radarPlotly <- renderPlotly({
        if(radarAttributes() %>% is.null()){
          plotly_empty(
            type = "scatter", 
            mode = "markers",
            width = NULL,
            height = NULL
          ) %>%
            plotly::config(
              displayModeBar = FALSE
            ) %>%
            layout(
              autosize = TRUE,
              title = list(
                text = "Select a player to show visualization",
                yref = "paper",
                y = 0.5
              )
            )
        } else {
          
          data <- radarAttributes() %>% 
            pivot_longer(
              where(is.numeric),
              names_to = "attributeIndex",
              values_to = "Rating"
            ) %>% 
            mutate(
              text = paste(attributeIndex, Rating, sep = ": ")
            ) %>% 
            ## Adds a duplicated first observation to allow the lines to connect
            add_row(
              .[1,]
            )
          
          fig <- 
            plot_ly(
              data = data,
              r = 0,
              theta = ~attributeIndex,
              width = 400,
              height = 250
            ) %>% 
            add_trace(
              type = 'scatterpolar',
              mode = 'lines',
              r = ~Rating,
              theta = ~attributeIndex,
              text = ~text,
              fill = 'none',
              hoverinfo = "text",
              line = 
                list(
                  color = "#ffffff",
                  width = 3
                ),
              name = ~Name,
              data = data
            ) %>% 
            add_trace(
              type = 'barpolar',
              width = 360,
              hoverinfo = "none",
              r = 10,
              marker = 
                list(
                  color = "#B81D13"
                )
            ) %>% 
            add_trace(
              type = 'barpolar',
              width = 360,
              hoverinfo = "none",
              r = 5,
              marker = 
                list(
                  color = "#EFB700"
                )
            ) %>% 
            add_trace(
              type = 'barpolar',
              width = 360,
              hoverinfo = "none",
              r = 5,
              marker = 
                list(
                  color = "#008450"
                )
            ) 
            
          
          fig %>%
            plotly::config(
              modeBarButtonsToRemove =
                c("zoom", "pan2d", "zoomIn2d", "zoomOut2d", "autoScale2d",
                  "resetScale2d", "hoverClosestCartesian",
                  "hoverCompareCartesian", "toggleSpikelines"
                )
            ) %>%
            layout(
              autosize = TRUE,
              dragmode= FALSE,
              polar =
                list(
                  radialaxis =
                    list(
                      visible = FALSE,
                      range = c(0,20)
                    )
                ),
              ## Legend is put to false so the plot is the same size
              showlegend = FALSE,
              paper_bgcolor='#ffffff00',
              plot_bgcolor='#ffffff00'
            )
        }
      }
      )
    }
  )
}

