
###########################################################################
###########################################################################
###                                                                     ###
###                         PLAYER BUILDER TOOL                         ###
###                                                                     ###
###########################################################################
###########################################################################





#Right now put the queries in here. In the future put them somewhere else?
require(DBI)
require(dbplyr)
require(RSQLite)
con <- dbConnect(SQLite(), "../database/SSL_Database.db")


PlayerDataRegularSeason <- dbGetQuery(con, 
                                      "SELECT 
Name,
Club,
Season,
count(Matchday) as GamesPlayed,
sum(`Minutes Played`) as `Minutes Played` ,
sum(`Distance Run (km)`) as `Distance Run (km)` ,
avg(`Average Rating`) as `Average Rating` ,
sum(`Player of the Match`) as `Player of the Match` ,
sum(`Goals`) as Goals ,
sum(`Assists`) as Assists ,
sum(`xG`) as xG ,
sum(`Shots on Target`) as `Shots on Target` ,
sum(`Shots`) as Shots ,
sum(`Penalties Taken`) as `Penalties Taken` ,
sum(`Penalties Scored`) as `Penalties Scored` ,
sum(`Successful Passes`) as `Successful Passes` ,
sum(`Attempted Passes`) as `Attempted Passes` ,
avg(`Pass%`) as `Pass%` ,
sum(`Key Passes`) as `Key Passes` ,
sum(`Successful Crosses`) as `Successful Crosses` ,
sum(`Attempted Crosses`) as `Attempted Crosses` ,
avg(`Cross%`) as `Cross%` ,
sum(`Chances Created`) as `Chances Created` ,
sum(`Successful Headers`) as `Successful Headers` ,
sum(`Attempted Headers`) as `Attempted Headers` ,
avg(`Header%`) as `Header%` ,
sum(`Key Headers`) as `Key Headers` ,
sum(`Dribbles`) as Dribbles ,
sum(`Tackles Won`) as `Tackles Won` ,
sum(`Attempted Tackles`) as `Attempted Tackles` ,
avg(`Tackle%`) as `Tackle%` ,
sum(`Key Tackles`) as `Key Tackles` ,
sum(`Interceptions`) as Interceptions ,
sum(`Clearances`) as Clearances ,
sum(`Mistakes Leading to Goals`) as `Mistakes Leading to Goals` ,
sum(`Yellow Cards`) as `Yellow Cards` ,
sum(`Red Cards`) as `Red Cards` ,
sum(`Fouls`) as Fouls ,
sum(`Fouls Against`) as `Fouls Against` ,
sum(`Offsides`) as Offsides
  FROM Player_Game_Data  group by Name, Season"
)

PlayerDataPlayoff <- dbGetQuery(con,'SELECT 
Name,
Club,
Season,
count(Matchday) as GamesPlayed,
sum(`Minutes Played`) as `Minutes Played` ,
sum(`Distance Run (km)`) as `Distance Run (km)` ,
avg(`Average Rating`) as `Average Rating` ,
sum(`Player of the Match`) as `Player of the Match` ,
sum(`Goals`) as Goals ,
sum(`Assists`) as Assists ,
sum(`xG`) as xG ,
sum(`Shots on Target`) as `Shots on Target` ,
sum(`Shots`) as Shots ,
sum(`Penalties Taken`) as `Penalties Taken` ,
sum(`Penalties Scored`) as `Penalties Scored` ,
sum(`Successful Passes`) as `Successful Passes` ,
sum(`Attempted Passes`) as `Attempted Passes` ,
avg(`Pass%`) as `Pass%` ,
sum(`Key Passes`) as `Key Passes` ,
sum(`Successful Crosses`) as `Successful Crosses` ,
sum(`Attempted Crosses`) as `Attempted Crosses` ,
avg(`Cross%`) as `Cross%` ,
sum(`Chances Created`) as `Chances Created` ,
sum(`Successful Headers`) as `Successful Headers` ,
sum(`Attempted Headers`) as `Attempted Headers` ,
avg(`Header%`) as `Header%` ,
sum(`Key Headers`) as `Key Headers` ,
sum(`Dribbles`) as Dribbles ,
sum(`Tackles Won`) as `Tackles Won` ,
sum(`Attempted Tackles`) as `Attempted Tackles` ,
avg(`Tackle%`) as `Tackle%` ,
sum(`Key Tackles`) as `Key Tackles` ,
sum(`Interceptions`) as Interceptions ,
sum(`Clearances`) as Clearances ,
sum(`Mistakes Leading to Goals`) as `Mistakes Leading to Goals` ,
sum(`Yellow Cards`) as `Yellow Cards` ,
sum(`Red Cards`) as `Red Cards` ,
sum(`Fouls`) as Fouls ,
sum(`Fouls Against`) as `Fouls Against` ,
sum(`Offsides`) as Offsides
  FROM Player_Game_Data  where MatchDay LIKE  "%Cup%" group by Name, Season')


PlayerSumRegular <- dbGetQuery(con, 
                               "SELECT 
Name,
count(Matchday) as GamesPlayed,
sum(`Minutes Played`) as `Minutes Played` ,
sum(`Distance Run (km)`) as `Distance Run (km)` ,
avg(`Average Rating`) as `Average Rating` ,
sum(`Player of the Match`) as `Player of the Match` ,
sum(`Goals`) as Goals ,
sum(`Assists`) as Assists ,
sum(`xG`) as xG ,
sum(`Shots on Target`) as `Shots on Target` ,
sum(`Shots`) as Shots ,
sum(`Penalties Taken`) as `Penalties Taken` ,
sum(`Penalties Scored`) as `Penalties Scored` ,
sum(`Successful Passes`) as `Successful Passes` ,
sum(`Attempted Passes`) as `Attempted Passes` ,
avg(`Pass%`) as `Pass%` ,
sum(`Key Passes`) as `Key Passes` ,
sum(`Successful Crosses`) as `Successful Crosses` ,
sum(`Attempted Crosses`) as `Attempted Crosses` ,
avg(`Cross%`) as `Cross%` ,
sum(`Chances Created`) as `Chances Created` ,
sum(`Successful Headers`) as `Successful Headers` ,
sum(`Attempted Headers`) as `Attempted Headers` ,
avg(`Header%`) as `Header%` ,
sum(`Key Headers`) as `Key Headers` ,
sum(`Dribbles`) as Dribbles ,
sum(`Tackles Won`) as `Tackles Won` ,
sum(`Attempted Tackles`) as `Attempted Tackles` ,
avg(`Tackle%`) as `Tackle%` ,
sum(`Key Tackles`) as `Key Tackles` ,
sum(`Interceptions`) as Interceptions ,
sum(`Clearances`) as Clearances ,
sum(`Mistakes Leading to Goals`) as `Mistakes Leading to Goals` ,
sum(`Yellow Cards`) as `Yellow Cards` ,
sum(`Red Cards`) as `Red Cards` ,
sum(`Fouls`) as Fouls ,
sum(`Fouls Against`) as `Fouls Against` ,
sum(`Offsides`) as Offsides
  FROM Player_Game_Data  group by Name"
)


PlayerSumPlayoff <- dbGetQuery(con, 
                               "SELECT 
Name,
count(Matchday) as GamesPlayed,
sum(`Minutes Played`) as `Minutes Played` ,
sum(`Distance Run (km)`) as `Distance Run (km)` ,
avg(`Average Rating`) as `Average Rating` ,
sum(`Player of the Match`) as `Player of the Match` ,
sum(`Goals`) as Goals ,
sum(`Assists`) as Assists ,
sum(`xG`) as xG ,
sum(`Shots on Target`) as `Shots on Target` ,
sum(`Shots`) as Shots ,
sum(`Penalties Taken`) as `Penalties Taken` ,
sum(`Penalties Scored`) as `Penalties Scored` ,
sum(`Successful Passes`) as `Successful Passes` ,
sum(`Attempted Passes`) as `Attempted Passes` ,
avg(`Pass%`) as `Pass%` ,
sum(`Key Passes`) as `Key Passes` ,
sum(`Successful Crosses`) as `Successful Crosses` ,
sum(`Attempted Crosses`) as `Attempted Crosses` ,
avg(`Cross%`) as `Cross%` ,
sum(`Chances Created`) as `Chances Created` ,
sum(`Successful Headers`) as `Successful Headers` ,
sum(`Attempted Headers`) as `Attempted Headers` ,
avg(`Header%`) as `Header%` ,
sum(`Key Headers`) as `Key Headers` ,
sum(`Dribbles`) as Dribbles ,
sum(`Tackles Won`) as `Tackles Won` ,
sum(`Attempted Tackles`) as `Attempted Tackles` ,
avg(`Tackle%`) as `Tackle%` ,
sum(`Key Tackles`) as `Key Tackles` ,
sum(`Interceptions`) as Interceptions ,
sum(`Clearances`) as Clearances ,
sum(`Mistakes Leading to Goals`) as `Mistakes Leading to Goals` ,
sum(`Yellow Cards`) as `Yellow Cards` ,
sum(`Red Cards`) as `Red Cards` ,
sum(`Fouls`) as Fouls ,
sum(`Fouls Against`) as `Fouls Against` ,
sum(`Offsides`) as Offsides
  FROM Player_Game_Data  where MatchDay LIKE  \"%Cup%\" group by Name"
) 

KeeperDataRegularSeason <-dbGetQuery(con,
                                     "SELECT 
                                     Name,
                                     Club,
                                     Season,
                                     count(Matchday) as GamesPlayed,
                                     sum(`Minutes Played`) as `Minutes Played` ,
                                     avg(`Average Rating`) as `Average Rating` ,
                                     sum(`Player of the Match`) as `Player of the Match` ,
                                     sum(`Won`) as Won ,
                                     sum(`Lost`) as Lost ,
                                     sum(`Drawn`) as Drawn ,
                                     sum(`Clean Sheets`) as `Clean Sheets` ,
                                     sum(`Conceded`) as Conceded ,
                                     sum(`Saves Parried`) as `Saves Parried` ,
                                     sum(`Saves Held`) as `Saves Held` ,
                                     sum(`Saves Tipped`) as `Saves Tipped` ,
                                     avg(`Save%`) as `Save%` ,
                                     sum(`Penalties Faced`) as `Penalties Faced` ,
                                     sum(`Penalties Saved`) as `Penalties Saved` ,
                                     avg(`xSave%`) as `xSave%` ,
                                     sum(`Result`) as Result ,
                                     sum(`Opponent`) as Opponent ,
                                     sum(`Matchday`) as Matchday
                                     FROM Keeper_Game_Data group by Name, Season"                                
                                     
)


KeeperDataPlayoff <-dbGetQuery(con,
                               "SELECT 
                                     Name,
                                     Club,
                                     Season,
                                     count(Matchday) as GamesPlayed,
                                     sum(`Minutes Played`) as `Minutes Played` ,
                                     avg(`Average Rating`) as `Average Rating` ,
                                     sum(`Player of the Match`) as `Player of the Match` ,
                                     sum(`Won`) as Won ,
                                     sum(`Lost`) as Lost ,
                                     sum(`Drawn`) as Drawn ,
                                     sum(`Clean Sheets`) as `Clean Sheets` ,
                                     sum(`Conceded`) as Conceded ,
                                     sum(`Saves Parried`) as `Saves Parried` ,
                                     sum(`Saves Held`) as `Saves Held` ,
                                     sum(`Saves Tipped`) as `Saves Tipped` ,
                                     avg(`Save%`) as `Save%` ,
                                     sum(`Penalties Faced`) as `Penalties Faced` ,
                                     sum(`Penalties Saved`) as `Penalties Saved` ,
                                     avg(`xSave%`) as `xSave%` ,
                                     sum(`Result`) as Result ,
                                     sum(`Opponent`) as Opponent ,
                                     sum(`Matchday`) as Matchday  
                                     FROM Keeper_Game_Data  where  MatchDay like \"%Cup%\" group by Name, Season"                                
                               
)

KeeperSumPlayoff <-dbGetQuery(con,
                              "SELECT 
                                     Name,
                                     count(Matchday) as GamesPlayed,
                                     sum(`Minutes Played`) as `Minutes Played` ,
                                     avg(`Average Rating`) as `Average Rating` ,
                                     sum(`Player of the Match`) as `Player of the Match` ,
                                     sum(`Won`) as Won ,
                                     sum(`Lost`) as Lost ,
                                     sum(`Drawn`) as Drawn ,
                                     sum(`Clean Sheets`) as `Clean Sheets` ,
                                     sum(`Conceded`) as Conceded ,
                                     sum(`Saves Parried`) as `Saves Parried` ,
                                     sum(`Saves Held`) as `Saves Held` ,
                                     sum(`Saves Tipped`) as `Saves Tipped` ,
                                     avg(`Save%`) as `Save%` ,
                                     sum(`Penalties Faced`) as `Penalties Faced` ,
                                     sum(`Penalties Saved`) as `Penalties Saved` ,
                                     avg(`xSave%`) as `xSave%` ,
                                     sum(`Result`) as Result ,
                                     sum(`Opponent`) as Opponent ,
                                     sum(`Matchday`) as Matchday
                                     FROM Keeper_Game_Data where  MatchDay like \"%Cup%\" group by Name"                                
                              
)

KeeperSumRegular <-dbGetQuery(con,
                              "SELECT 
                                     Name,
                                     count(Matchday) as GamesPlayed,
                                     sum(`Minutes Played`) as `Minutes Played` ,
                                     avg(`Average Rating`) as `Average Rating` ,
                                     sum(`Player of the Match`) as `Player of the Match` ,
                                     sum(`Won`) as Won ,
                                     sum(`Lost`) as Lost ,
                                     sum(`Drawn`) as Drawn ,
                                     sum(`Clean Sheets`) as `Clean Sheets` ,
                                     sum(`Conceded`) as Conceded ,
                                     sum(`Saves Parried`) as `Saves Parried` ,
                                     sum(`Saves Held`) as `Saves Held` ,
                                     sum(`Saves Tipped`) as `Saves Tipped` ,
                                     avg(`Save%`) as `Save%` ,
                                     sum(`Penalties Faced`) as `Penalties Faced` ,
                                     sum(`Penalties Saved`) as `Penalties Saved` ,
                                     avg(`xSave%`) as `xSave%` ,
                                     sum(`Result`) as Result ,
                                     sum(`Opponent`) as Opponent ,
                                     sum(`Matchday`) as Matchday 
                                     FROM Keeper_Game_Data group by Name"                                
                              
)



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
          tabPanel(
            "Player Updater",
            
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
                  h4("TPE Information", align = "center"),
                  fluidRow(
                    column(
                      width = 10,
                      offset = 1,
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
              
              ##------------------------------------
              ##  Right hand side with the builder  
              ##------------------------------------
              column(
                width = 8,
                p("Edit the Value column by double clicking on the cell of the attribute you want to edit. 
                Clicking on export provides a formatted text with all the updates you have made on your build."),
                DTOutput(ns("attributeTable"), width = "80%")
              )
              ##------------------------------------
            )
          ),
          tabPanel(
            "Game Log",
            fluidRow(
              column(
                width = 4,
                selectizeInput(
                  inputId = ns("gameSeason"),
                  label = "Filter Season",
                  choices =
                    keeperGameData$Season %>%
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
                      "Cup",
                      "League"
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
          )
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
            -Keeper
          )
      })
      
      ##---------------------------------------------------------------
      ##                          Observers                           -
      ##---------------------------------------------------------------
      
      ## Observer that calculates the correct used TPE and available TPE for the build
      observeEvent(
        input$currentTPE,
        {
          currentAvailable(input$currentTPE - sum(reactives$currentBuild$cost))
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
          
          currentAvailable(input$currentTPE - sum(reactives$currentBuild$cost))
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
          
          currentAvailable(input$currentTPE - sum(reactives$currentBuild$cost))
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
                          input$currentTPE-playerData %>% filter(Name == input$player) %>% select(TPE) %>% unlist() %>% unname(),
                          "=",
                          input$currentTPE),
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
          ),
          fluidRow(
            column(
              width = 6,
              h5("Forum TPE")
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
                inputId = session$ns("currentTPE"),
                label = NULL,
                value = playerData %>% filter(Name == input$player) %>% select(TPE) %>% unlist() %>% unname(),
                min = 350,
                max = 2500,
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
      
      # Using reactable for Game Logs
      output$GameData <- renderReactable({
        if(any(reactives$currentBuild$Group == "Goalkeeper")){
          data <- 
            keeperGameData %>% 
            filter(
              Name == input$player
            )
        } else {
          data <- 
            playerGameData %>% 
            filter(
              Name == input$player
            ) %>% 
            mutate(
              `Average Rating` = round(`Average Rating`, 2),
              xG = round(xG, 2)
            ) 
        }
        
        if(!is.null(input$gameSeason)){
          data <- 
            data %>% 
            filter(
              Season %in% input$gameSeason
            )
        }
        
        if(!is.null(input$gameFilter)){
          if("Cup" %in% input$gameFilter){
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
              Matchday,
              Club,
              Opponent,
              Result
            ),
            .before = `Minutes Played` 
          ) %>% 
          reactable(
            pagination = FALSE,
            columns = 
              list(
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
                Opponent = 
                  colDef(
                    name = "Opp.",
                    maxWidth = 50,
                    class = "cell",
                    align = "center",
                    cell = function(value, index){
                      
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
                        
                        if(any(str_detect(values, pattern = "p"))){
                          
                          if(which(str_detect(values, pattern = "p")) == 1){
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
          if("Cup" %in% input$careerFilter){
            if(any(reactives$currentBuild$Group == "Goalkeeper")){
              data <- 
                KeeperDataPlayoff %>% 
                filter(
                  Name == input$player
                )
              dataSum <- 
                KeeperSumPlayoff %>%
                filter(
                  Name == input$player
                )
              
              data %>% 
                reactable(
                  pagination = FALSE,
                  columns = 
                    list(
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
                      `Name` = colDef(footer = "Totals"),
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
                      `GamesPlayed` = colDef(footer = dataSum$`GamesPlayed`)
                      
                    ),
                  defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
                )
              
              
              
            } else {
              data <- 
                PlayerDataPlayoff %>% 
                filter(
                  Name == input$player
                ) %>% 
                mutate(
                  `Average Rating` = round(`Average Rating`, 2),
                  xG = round(xG, 2)
                ) 
              dataSum <- 
                PlayerSumPlayoff %>% 
                filter(
                  Name == input$player
                ) %>% 
                mutate(
                  `Average Rating` = round(`Average Rating`, 2),
                  xG = round(xG, 2)
                )
              
              data %>% 
                reactable(
                  pagination = FALSE,
                  columns = 
                    list(
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
                      `Name` = colDef(footer = "Totals"),
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
                      `GamesPlayed` = colDef(footer = dataSum$`GamesPlayed`)
                      
                    ),
                  defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
                )
              
              
              
            }
          } else {
            if(any(reactives$currentBuild$Group == "Goalkeeper")){
              KeeperDataRegularSeason %>% 
                filter(
                  Name == input$player
                )
              data <- 
                KeeperDataRegularSeason %>% 
                filter(
                  Name == input$player
                )
              dataSum <- 
                KeeperSumRegular %>%
                filter(
                  Name == input$player
                )
              
              
              data %>% 
                reactable(
                  pagination = FALSE,
                  columns = 
                    list(
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
                      `Name` = colDef(footer = "Totals"),
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
                      `GamesPlayed` = colDef(footer = dataSum$`GamesPlayed`)
                      
                    ),
                  defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
                )
              
              
            } else {
              data <- 
                PlayerDataRegularSeason %>% 
                filter(
                  Name == input$player
                ) %>% 
                mutate(
                  `Average Rating` = round(`Average Rating`, 2),
                  xG = round(xG, 2)
                ) 
              
              dataSum <- 
                PlayerSumRegular %>% 
                filter(
                  Name == input$player
                ) %>% 
                mutate(
                  `Average Rating` = round(`Average Rating`, 2),
                  xG = round(xG, 2)
                )
              
              data %>% 
                reactable(
                  pagination = FALSE,
                  columns = 
                    list(
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
                      `Name` = colDef(footer = "Totals"),
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
                      `GamesPlayed` = colDef(footer = dataSum$`GamesPlayed`)

                    ),
                  defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
                )
              
              
            }
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

