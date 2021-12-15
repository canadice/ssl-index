
###########################################################################
###########################################################################
###                                                                     ###
###                 INDIVIDUAL STATS FOR THE SSL INDEX                  ###
###                                                                     ###
###########################################################################
###########################################################################


### UI module for player similarities using MDS
playerStatsUI <- function(id){
  
  ## Creating the namespacing function for all IDs
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 10,
          offset = 1,
          tabBox(
            width = NULL,
            selected = "Player Stats",
            tabPanel(
              "Player Stats",
              h4("Outfield", align = "center"),
              DT::DTOutput(
                outputId = ns("playerStats")
              ),
              h4("Goalkeeper", align = "center"),
              DT::DTOutput(
                outputId = ns("goalieStats")
              )
            ),
            tabPanel(
              "Leaders",
              fluidRow(
                column(
                  width = 5,
                  offset = 1,
                  DT::DTOutput(
                    outputId = ns("leaderGoals")
                  )
                ),
                column(
                  width = 5,
                  DT::DTOutput(
                    outputId = ns("leaderAssists")
                  )
                )
              ),
              br(),
              fluidRow(
                column(
                  width = 5,
                  offset = 1,
                  DT::DTOutput(
                    outputId = ns("leaderSoT")
                  )
                ),
                column(
                  width = 5,
                  DT::DTOutput(
                    outputId = ns("leaderInterceptions")
                  )
                )
              ),
              br(),
              fluidRow(
                column(
                  width = 5,
                  offset = 1,
                  DT::DTOutput(
                    outputId = ns("leaderFouls")
                  )
                ),
                column(
                  width = 5,
                  DT::DTOutput(
                    outputId = ns("leaderPoM")
                  )
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
playerStatsSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){

      ###  Reading the data
           
      outfieldUrl <- "https://raw.githack.com/canadice/ssl-index/main/SSL-Index/data/playerStats.html"
      goalieUrl <- "https://raw.githack.com/canadice/ssl-index/main/SSL-Index/data/goalieStats.html"
      
      ###  Processing the data                                         
      ## Outfield
      {
      playerStats <- 
        outfieldUrl %>% 
        read_html() %>% 
        html_elements("table") %>% 
        html_table() %>% 
        .[[1]] %>% 
        rename(
          Appearances = Apps,
          Goals = Gls,
          Assists = Ast,
          `Attempted Passes` = `Pas A`,
          `Successful Passes` = `Ps C`,
          `Key Passes` = `K Pas`,
          `Successful Crosses` = `Cr C`,
          `Attempted Crosses` = `Cr A`,
          `Chances Created` = CCC,
          `Tackles Won` = `Tck W`,
          `Tackle%` = `Tck R`,
          `Key Tackles` = `K Tck`,
          `Successful Headers` = Hdrs,
          `Attempted Headers` = `Hdrs A`,
          `Header%` = `Hdr %`,
          `Key Headers` = `K Hdrs`,
          `Shots on Target` = ShT,
          `Mistakes Leading to Goals` = `Gl Mst`,
          Dribbles = `Drb`,
          Offsides = Off,
          `Fouls Against` = FA,
          Interceptions = Itc,
          `Yellow Cards` = Yel,
          `Red Cards` = Red,
          Fouls = Fls,
          `Penalties Taken` = Pens,
          `Penalties Scored` = `Pens S`,
          `Distance Run (km)` = Distance,
          `Average Rating` = `Av Rat`,
          `Player of the Match` = `PoM`
        ) %>% 
        mutate(
          `Minutes per Game` = (Mins %>% as.numeric()/Appearances %>% as.numeric()) %>% round(2),
          `Pass%` = (`Successful Passes` %>% as.numeric()/`Attempted Passes` %>% as.numeric()) %>% round(4)*100,
          # Position = NA,
          Nationality = 
            Name %>% 
            str_split(
              pattern = " - ", 
              simplify = TRUE
            ) %>% 
            .[,2],
          Name = 
            Name %>% 
            str_split(
              pattern = " - ", 
              simplify = TRUE
            ) %>% 
            .[,1],
          # `Pass%` = 
          #   `Pass%` %>% 
          #   str_remove_all(
          #     pattern = "%"
          #   ) %>% 
          #   as.numeric(),
          `Header%` =
            `Header%` %>% 
            str_remove_all(
              pattern = "%"
            ) %>% 
            as.numeric(),
          `Tackle%` =
            `Tackle%` %>% 
            str_remove_all(
              pattern = "%"
            ) %>% 
            as.numeric(),
          `Distance Run (km)` = 
            `Distance Run (km)` %>% 
            str_remove_all(
              pattern = "km"
            ) %>% 
            as.numeric(),
          Club =
            Club %>%
            str_split(pattern = "-", simplify = TRUE) %>% 
            .[,1] %>% 
            str_squish()
        ) %>% 
        mutate(
          across(
            !contains(
              c("Name", "Information", "Nationality", "Position", "Club")
            ),
            as.numeric
          )
        ) %>% 
        relocate(
          c(
            Nationality,
            Position,
            Appearances,
            `Minutes per Game`,
            `Distance Run (km)`
          ),
          .after = Name
        ) %>% 
        relocate(
          `Pass%`,
          .after = `Successful Passes`
        ) %>% 
        select(
          -`Inf`,
          -Mins,
          -`Tck A`,
          -Rec
        ) %>% 
        arrange(
          `Average Rating` %>% desc()
        ) %>% 
        # As there are non-numeric values being transformed correctly to NA, warnings are suppressed. 
        suppressWarnings()
      }
      ## Goalkeeper
      {
      goalieStats <- 
        goalieUrl %>% 
        read_html() %>% 
        html_elements("table") %>% 
        html_table() %>% 
        .[[1]] %>% 
        rename(
          Appearances = Apps,
          Drawn = D,
          Conceded = Conc,
          `Saves Parried` = Svp,
          `Saves Held`= Svh,
          `Saves Tipped` = Svt,
          `Penalties Saved` = `Pens Saved`,
          `Penalties Faced` = `Pens Faced`,
          `Average Rating` = `Av Rat`,
          `Player of the Match` = `PoM`
        ) %>% 
        mutate(
          `Minutes per Game` = (Mins %>% as.numeric()/Appearances %>% as.numeric()) %>% round(2),
          Nationality = 
            Name %>% 
            str_split(
              pattern = " - ", 
              simplify = TRUE
            ) %>% 
            .[,2],
          Name = 
            Name %>% 
            str_split(
              pattern = " - ", 
              simplify = TRUE
            ) %>% 
            .[,1],
          Club =
            Club %>%
            str_split(pattern = "-", simplify = TRUE) %>% 
            .[,1] %>% 
            str_squish()
        ) %>% 
        mutate(
          across(
            !contains(
              c("Name", "Information", "Nationality", "Position", "Club")
            ),
            as.numeric
          )
        ) %>% 
        mutate(
          `Save%` = ((`Saves Parried`+`Saves Held`+`Saves Tipped`)/(`Saves Parried`+`Saves Held`+`Saves Tipped`+Conceded)) %>% round(4) * 100
        ) %>% 
        relocate(
          `Save%`,
          .after = `Saves Tipped`
        ) %>% 
        relocate(
          c(
            Nationality,
            Position,
            Appearances,
            `Minutes per Game`,
          ),
          .after = Name
        ) %>% 
        select(
          -`Inf`,
          -Mins,
          -Rec
        ) %>% 
        arrange(
          `Average Rating` %>% desc()
        ) %>% 
        # As there are non-numeric values being transformed correctly to NA, warnings are suppressed. 
        suppressWarnings()
      }
      
      output$playerStats <- renderDT({
        if(length(playerStats) == 0){
          NULL
        } else {
          datatable(
            playerStats,
            style = "bootstrap",
            class = 'compact cell-border stripe',
            rownames = FALSE,
            escape = FALSE,
            options = 
              list(
                ordering = TRUE, 
                ## Sets a scroller for the rows
                scrollY = '80%',
                sScrollX = "100%",
                ## Sets size of rows shown
                scrollCollapse = TRUE,
                pageLength = 10,
                dom = 'Rftp',
                ## Sets color of table background
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#00044d', 'color': '#fff'});",
                  "}")
              )
          )  
        }
      })
      
      output$goalieStats <- renderDT({
        if(length(goalieStats) == 0){
          NULL
        } else {
          datatable(
            goalieStats,
            style = "bootstrap",
            class = 'compact cell-border stripe',
            rownames = FALSE,
            escape = FALSE,
            options = 
              list(
                ordering = TRUE, 
                ## Sets a scroller for the rows
                scrollY = '80%',
                sScrollX = "100%",
                ## Sets size of rows shown
                scrollCollapse = TRUE,
                pageLength = 10,
                dom = 'Rftp',
                ## Sets color of table background
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#00044d', 'color': '#fff'});",
                  "}")
              )
          )  
        }
      })
      
      
      output$leaderGoals <- renderDT({
        if(length(playerStats) == 0){
          NULL
        } else {
          datatable(
            playerStats %>% 
              select(Name, Goals) %>% 
              arrange(-Goals) %>% 
              slice_head(n = 10),
            style = "bootstrap",
            class = 'compact cell-border stripe',
            rownames = FALSE,
            escape = FALSE,
            options = 
              list(
                ordering = FALSE, 
                ## Sets size of rows shown
                scrollCollapse = TRUE,
                paging = FALSE,
                dom = 't',
                ## Sets color of table background
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#00044d', 'color': '#fff'});",
                  "}")
              )
          )  
        }
      })
      
      output$leaderAssists <- renderDT({
        if(length(playerStats) == 0){
          NULL
        } else {
          datatable(
            playerStats %>% 
              select(Name, Assists) %>% 
              arrange(-Assists) %>% 
              slice_head(n = 10),
            style = "bootstrap",
            class = 'compact cell-border stripe',
            rownames = FALSE,
            escape = FALSE,
            options = 
              list(
                ordering = FALSE, 
                ## Sets size of rows shown
                scrollCollapse = TRUE,
                paging = FALSE,
                dom = 't',
                ## Sets color of table background
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#00044d', 'color': '#fff'});",
                  "}")
              )
          )  
        }
      })
      
      output$leaderSoT <- renderDT({
        if(length(playerStats) == 0){
          NULL
        } else {
          datatable(
            playerStats %>% 
              select(Name, `Shots on Target`) %>% 
              arrange(-`Shots on Target`) %>% 
              slice_head(n = 10),
            style = "bootstrap",
            class = 'compact cell-border stripe',
            rownames = FALSE,
            escape = FALSE,
            options = 
              list(
                ordering = FALSE, 
                ## Sets size of rows shown
                scrollCollapse = TRUE,
                paging = FALSE,
                dom = 't',
                ## Sets color of table background
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#00044d', 'color': '#fff'});",
                  "}")
              )
          )  
        }
      })
      
      output$leaderInterceptions <- renderDT({
        if(length(playerStats) == 0){
          NULL
        } else {
          datatable(
            playerStats %>% 
              select(Name, Interceptions) %>% 
              arrange(-Interceptions) %>% 
              slice_head(n = 10),
            style = "bootstrap",
            class = 'compact cell-border stripe',
            rownames = FALSE,
            escape = FALSE,
            options = 
              list(
                ordering = FALSE, 
                ## Sets size of rows shown
                scrollCollapse = TRUE,
                paging = FALSE,
                dom = 't',
                ## Sets color of table background
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#00044d', 'color': '#fff'});",
                  "}")
              )
          )  
        }
      })
      
      output$leaderPoM <- renderDT({
        if(length(playerStats) == 0){
          NULL
        } else {
          datatable(
            playerStats %>% 
              select(Name, `Player of the Match`) %>% 
              arrange(-`Player of the Match`) %>% 
              slice_head(n = 10),
            style = "bootstrap",
            class = 'compact cell-border stripe',
            rownames = FALSE,
            escape = FALSE,
            options = 
              list(
                ordering = FALSE, 
                ## Sets size of rows shown
                scrollCollapse = TRUE,
                paging = FALSE,
                dom = 't',
                ## Sets color of table background
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#00044d', 'color': '#fff'});",
                  "}")
              )
          )  
        }
      })
      
      output$leaderFouls <- renderDT({
        if(length(playerStats) == 0){
          NULL
        } else {
          datatable(
            playerStats %>% 
              select(Name, Fouls) %>% 
              arrange(-Fouls) %>% 
              slice_head(n = 10),
            style = "bootstrap",
            class = 'compact cell-border stripe',
            rownames = FALSE,
            escape = FALSE,
            options = 
              list(
                ordering = FALSE, 
                ## Sets size of rows shown
                scrollCollapse = TRUE,
                paging = FALSE,
                dom = 't',
                ## Sets color of table background
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': '#00044d', 'color': '#fff'});",
                  "}")
              )
          )  
        }
      })
    }
  )
}

