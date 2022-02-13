
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
          width = 2,
          selectInput(
            inputId = ns("season"),
            label = "Select season",
            choices = 
              unique(
                playerData$Class
              ) %>% 
              str_extract(pattern = "[0-9]+") %>% 
              unname() %>% 
              sort(decreasing = TRUE)
          )
        )
      ),
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
              "Outfield Leaders",
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
            ),
            tabPanel(
              "Goalkeeper Leaders",
              fluidRow(
                column(
                  width = 5,
                  offset = 1,
                  DT::DTOutput(
                    outputId = ns("leaderWins")
                  )
                ),
                column(
                  width = 5,
                  DT::DTOutput(
                    outputId = ns("leaderSavePerc")
                  )
                )
              ),
              br(),
              fluidRow(
                column(
                  width = 5,
                  offset = 1,
                  DT::DTOutput(
                    outputId = ns("leaderCleanSheets")
                  )
                ),
                column(
                  width = 5,
                  DT::DTOutput(
                    outputId = ns("leaderConceded")
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
      playerStats <- reactive({
        outfieldUrl <-
          paste(
            "https://raw.githack.com/canadice/ssl-index/main/SSL-Index/data/S",
            input$season,
            "_playerStats.html",
            sep = ""
          )
        
        outfieldUrl %>% 
          read_html() %>% 
          html_elements("table") %>% 
          html_table() %>% 
          .[[1]] %>% 
          rename(
            Apps = Apps,
            Goals = Gls,
            Assists = Ast,
            `Minutes Played` = Mins,
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
            across(
              c(
                `Minutes Played`:`Average Rating`
              ),
              .fns = str_replace_all,
              pattern = "[^\\d\\.]+",
              replacement = ""
            )
          ) %>% 
          mutate(
            Name = 
              case_when(
                str_detect(Name, "GFuel") ~ "FazeBerry GFuel - American",
                TRUE ~ Name
              )
          ) %>% 
          mutate(
            `Pass%` = (`Successful Passes` %>% as.numeric()/`Attempted Passes` %>% as.numeric()) %>% round(4)*100,
            `Header%` = (`Successful Headers` %>% as.numeric()/`Attempted Headers` %>% as.numeric()) %>% round(4)*100,
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
            `Cross%` = (`Successful Crosses` %>% as.numeric()/`Attempted Crosses` %>% as.numeric()) %>% round(4)*100,
            # `Header%` =
            #   `Header%` %>% 
            #   as.numeric(),
            `Tackle%` =
              `Tackle%` %>% 
              as.numeric(),
            `Distance Run (km)` = 
              `Distance Run (km)` %>% 
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
            ),
            `Attempted Tackles` = ((`Tackles Won` %>% as.numeric())/(`Tackle%`/100)) %>% round(0)
          ) %>% 
          relocate(
            c(
              Nationality,
              Position,
              Apps,
              `Minutes Played`,
              `Distance Run (km)`
            ),
            .after = Name
          ) %>% 
          relocate(
            c(
              `Attempted Passes`,
              `Pass%`
            ),
            .after = `Successful Passes`
          ) %>% 
          relocate(
            c(
              `Attempted Crosses`,
              `Cross%`
            ),
            .after = `Successful Crosses`
          ) %>% 
          relocate(
            c(
              `Attempted Headers`
            ),
            .after = `Successful Headers`
          ) %>% 
          relocate(
            c(
              `Attempted Tackles`,
              `Tackle%`
            ),
            .after = `Tackles Won`
          ) %>% 
          relocate(
            `Shots on Target`,
            .before = `Shots`
          ) %>% 
          select(
            -`Inf`,
            -`Tck A`,
            -Rec
          ) %>% 
          arrange(
            `Average Rating` %>% desc()
          ) %>% 
          # As there are non-numeric values being transformed correctly to NA, warnings are suppressed. 
          suppressWarnings()
      })
      
      
      goalieStats <- reactive({
        goalieUrl <-
          paste(
            "https://raw.githack.com/canadice/ssl-index/main/SSL-Index/data/S",
            input$season,
            "_goalieStats.html",
            sep = ""
          )
        
        goalieUrl %>% 
          read_html() %>% 
          html_elements("table") %>% 
          html_table() %>% 
          .[[1]] %>% 
          rename(
            Apps = Apps,
            `Minutes Played` = Mins,
            Drawn = D,
            Conceded = Conc,
            `Saves Parried` = Svp,
            `Saves Held`= Svh,
            `Saves Tipped` = Svt,
            `Penalties Saved` = `Pens Saved`,
            `Penalties Faced` = `Pens Faced`,
            `Average Rating` = `Av Rat`,
            `Player of the Match` = `PoM`,
            `Clean Sheets` = Shutouts
          ) %>% 
          mutate(
            across(
              c(
                `Minutes Played`:`Average Rating`
              ),
              .fns = str_replace_all,
              pattern = "[^\\d\\.]+",
              replacement = ""
            )
          ) %>% 
          mutate(
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
              Apps,
              `Minutes Played`,
            ),
            .after = Name
          ) %>% 
          select(
            -`Inf`,
            -Rec
          ) %>% 
          arrange(
            `Average Rating` %>% desc()
          ) %>% 
          # As there are non-numeric values being transformed correctly to NA, warnings are suppressed. 
          suppressWarnings()
      })
           
      
      output$playerStats <- renderDT({
        if(length(playerStats()) == 0){
          NULL
        } else {
          datatable(
            playerStats(),
            style = "bootstrap",
            class = 'compact cell-border stripe',
            rownames = FALSE,
            escape = FALSE,
            extensions = "FixedColumns",
            options = 
              list(
                ordering = TRUE,
                ## Sets a scroller for the rows
                scrollY = "80%",
                scrollX = TRUE,
                ## Sets size of rows shown
                scrollCollapse = TRUE,
                pageLength = 15,
                dom = 'Rftp',
                fixedColumns = 
                  list(
                    leftColumns = 1
                  ),
                ## Tries to set the widths of the columns 
                autoWidth = TRUE,
                columnDefs = 
                  list(
                    # Name
                    list(
                      width = '150', 
                      targets = list(0,6)
                    ),
                    # Position and Club
                    list(
                      width = '100', 
                      targets = list(2,6)
                    )
                  )
              )
          )  
        }
      })
      
      output$goalieStats <- renderDT({
        if(length(goalieStats()) == 0){
          NULL
        } else {
          datatable(
            goalieStats(),
            style = "bootstrap",
            class = 'compact cell-border stripe',
            rownames = FALSE,
            escape = FALSE,
            extensions = "FixedColumns",
            options = 
              list(
                ordering = TRUE, 
                ## Sets a scroller for the rows
                scrollY = "80%",
                scrollX = TRUE,
                ## Sets size of rows shown
                scrollCollapse = TRUE,
                pageLength = 20,
                dom = 'Rftp',
                fixedColumns = 
                  list(
                    leftColumns = 1
                  ),
                ## Tries to set the widths of the columns. Note that 0 is the first column index.
                autoWidth = TRUE,
                columnDefs = 
                  list(
                    # Name
                    list(
                      width = '150', 
                      targets = list(0)
                    ),
                    # Club
                    list(
                      width = '100', 
                      targets = list(5)
                    )
                  )
              )
          )  
        }
      })
      
      
      output$leaderGoals <- renderDT({
        if(length(playerStats()) == 0){
          NULL
        } else {
          datatable(
            playerStats() %>% 
              select(Name, Goals, Club) %>% 
              left_join(
                teamInfo %>% 
                  select(
                    team, 
                    color.primary,
                    color.secondary
                  ),
                by = c("Club" = "team")
              ) %>% 
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
                columnDefs = 
                  list(
                    list(
                      targets = 2:4,
                      visible = FALSE
                    )
                  )
              )
          ) %>% 
            formatStyle(
              columns = 1:2,
              valueColumns = "color.primary",
              backgroundColor = 
                styleEqual(
                  sort(unique(teamInfo$color.primary)), 
                  sort(unique(teamInfo$color.primary))
                )
            ) %>% 
            formatStyle(
              columns = 1:2,
              valueColumns = "color.secondary",
              color = 
                styleEqual(
                  sort(unique(teamInfo$color.secondary)), 
                  sort(unique(teamInfo$color.secondary))
                )
            ) 
        }
      })
      
      output$leaderAssists <- renderDT({
        if(length(playerStats()) == 0){
          NULL
        } else {
          datatable(
            playerStats() %>% 
              select(Name, Assists, Club) %>% 
              left_join(
                teamInfo %>% 
                  select(
                    team, 
                    color.primary,
                    color.secondary
                  ),
                by = c("Club" = "team")
              ) %>% 
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
                columnDefs = 
                  list(
                    list(
                      targets = 2:4,
                      visible = FALSE
                    )
                  )
              )
          ) %>% 
            formatStyle(
              columns = 1:2,
              valueColumns = "color.primary",
              backgroundColor = 
                styleEqual(
                  sort(unique(teamInfo$color.primary)), 
                  sort(unique(teamInfo$color.primary))
                )
            ) %>% 
            formatStyle(
              columns = 1:2,
              valueColumns = "color.secondary",
              color = 
                styleEqual(
                  sort(unique(teamInfo$color.secondary)), 
                  sort(unique(teamInfo$color.secondary))
                )
            ) 
        }
      })
      
      output$leaderSoT <- renderDT({
        if(length(playerStats()) == 0){
          NULL
        } else {
          datatable(
            playerStats() %>% 
              select(Name, `Shots on Target`, Club) %>% 
              left_join(
                teamInfo %>% 
                  select(
                    team, 
                    color.primary,
                    color.secondary
                  ),
                by = c("Club" = "team")
              ) %>% 
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
                columnDefs = 
                  list(
                    list(
                      targets = 2:4,
                      visible = FALSE
                    )
                  )
              )
          ) %>% 
            formatStyle(
              columns = 1:2,
              valueColumns = "color.primary",
              backgroundColor = 
                styleEqual(
                  sort(unique(teamInfo$color.primary)), 
                  sort(unique(teamInfo$color.primary))
                )
            ) %>% 
            formatStyle(
              columns = 1:2,
              valueColumns = "color.secondary",
              color = 
                styleEqual(
                  sort(unique(teamInfo$color.secondary)), 
                  sort(unique(teamInfo$color.secondary))
                )
            )
        }
      })
      
      output$leaderInterceptions <- renderDT({
        if(length(playerStats()) == 0){
          NULL
        } else {
          datatable(
            playerStats() %>% 
              select(Name, Interceptions, Club) %>% 
              left_join(
                teamInfo %>% 
                  select(
                    team, 
                    color.primary,
                    color.secondary
                  ),
                by = c("Club" = "team")
              ) %>% 
              arrange(-Interceptions) %>% 
              slice_head(n = 10),
            style = "bootstrap",
            class = 'compact cell-border stripe',
            rownames = FALSE,
            escape = FALSE,
            options = 
              list(
                ordering = TRUE, 
                ## Sets size of rows shown
                scrollCollapse = TRUE,
                paging = FALSE,
                dom = 't',
                columnDefs = 
                  list(
                    list(
                      targets = 2:4,
                      visible = FALSE
                    )
                  )
              )
          ) %>% 
            formatStyle(
              columns = 1:2,
              valueColumns = "color.primary",
              backgroundColor = 
                styleEqual(
                  sort(unique(teamInfo$color.primary)), 
                  sort(unique(teamInfo$color.primary))
                )
            ) %>% 
            formatStyle(
              columns = 1:2,
              valueColumns = "color.secondary",
              color = 
                styleEqual(
                  sort(unique(teamInfo$color.secondary)), 
                  sort(unique(teamInfo$color.secondary))
                )
            )
        }
      })
      
      output$leaderPoM <- renderDT({
        if(length(playerStats()) == 0){
          NULL
        } else {
          datatable(
            playerStats() %>% 
              select(Name, `Player of the Match`, Club) %>% 
              left_join(
                teamInfo %>% 
                  select(
                    team, 
                    color.primary,
                    color.secondary
                  ),
                by = c("Club" = "team")
              ) %>%  
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
                columnDefs = 
                  list(
                    list(
                      targets = 2:4,
                      visible = FALSE
                    )
                  )
              )
          ) %>% 
            formatStyle(
              columns = 1:2,
              valueColumns = "color.primary",
              backgroundColor = 
                styleEqual(
                  sort(unique(teamInfo$color.primary)), 
                  sort(unique(teamInfo$color.primary))
                )
            ) %>% 
            formatStyle(
              columns = 1:2,
              valueColumns = "color.secondary",
              color = 
                styleEqual(
                  sort(unique(teamInfo$color.secondary)), 
                  sort(unique(teamInfo$color.secondary))
                )
            )
        }
      })
      
      output$leaderFouls <- renderDT({
        if(length(playerStats()) == 0){
          NULL
        } else {
          datatable(
            playerStats() %>% 
              select(Name, Fouls, Club) %>% 
              left_join(
                teamInfo %>% 
                  select(
                    team, 
                    color.primary,
                    color.secondary
                  ),
                by = c("Club" = "team")
              ) %>%  
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
                columnDefs = 
                  list(
                    list(
                      targets = 2:4,
                      visible = FALSE
                    )
                  )
              )
          ) %>% 
            formatStyle(
              columns = 1:2,
              valueColumns = "color.primary",
              backgroundColor = 
                styleEqual(
                  sort(unique(teamInfo$color.primary)), 
                  sort(unique(teamInfo$color.primary))
                )
            ) %>% 
            formatStyle(
              columns = 1:2,
              valueColumns = "color.secondary",
              color = 
                styleEqual(
                  sort(unique(teamInfo$color.secondary)), 
                  sort(unique(teamInfo$color.secondary))
                )
            )
        }
      })
      
      output$leaderSavePerc <- renderDT({
        if(length(goalieStats()) == 0){
          NULL
        } else {
          datatable(
            goalieStats() %>% 
              select(Name, `Save%`, Club) %>% 
              left_join(
                teamInfo %>% 
                  select(
                    team, 
                    color.primary,
                    color.secondary
                  ),
                by = c("Club" = "team")
              ) %>%  
              arrange(-`Save%`) %>% 
              slice_head(n = 5),
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
                columnDefs = 
                  list(
                    list(
                      targets = 2:4,
                      visible = FALSE
                    )
                  )
              )
          ) %>% 
            formatStyle(
              columns = 1:2,
              valueColumns = "color.primary",
              backgroundColor = 
                styleEqual(
                  sort(unique(teamInfo$color.primary)), 
                  sort(unique(teamInfo$color.primary))
                )
            ) %>% 
            formatStyle(
              columns = 1:2,
              valueColumns = "color.secondary",
              color = 
                styleEqual(
                  sort(unique(teamInfo$color.secondary)), 
                  sort(unique(teamInfo$color.secondary))
                )
            )
        }
      })
      
      output$leaderWins <- renderDT({
        if(length(goalieStats()) == 0){
          NULL
        } else {
          datatable(
            goalieStats() %>% 
              select(Name, Won, Club) %>% 
              left_join(
                teamInfo %>% 
                  select(
                    team, 
                    color.primary,
                    color.secondary
                  ),
                by = c("Club" = "team")
              ) %>%  
              arrange(-Won) %>% 
              slice_head(n = 5),
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
                columnDefs = 
                  list(
                    list(
                      targets = 2:4,
                      visible = FALSE
                    )
                  )
              )
          ) %>% 
            formatStyle(
              columns = 1:2,
              valueColumns = "color.primary",
              backgroundColor = 
                styleEqual(
                  sort(unique(teamInfo$color.primary)), 
                  sort(unique(teamInfo$color.primary))
                )
            ) %>% 
            formatStyle(
              columns = 1:2,
              valueColumns = "color.secondary",
              color = 
                styleEqual(
                  sort(unique(teamInfo$color.secondary)), 
                  sort(unique(teamInfo$color.secondary))
                )
            )
        }
      })
      
      output$leaderCleanSheets <- renderDT({
        if(length(goalieStats()) == 0){
          NULL
        } else {
          datatable(
            goalieStats() %>% 
              select(Name, `Clean Sheets`, Club) %>% 
              left_join(
                teamInfo %>% 
                  select(
                    team, 
                    color.primary,
                    color.secondary
                  ),
                by = c("Club" = "team")
              ) %>%  
              arrange(-`Clean Sheets`) %>% 
              slice_head(n = 5),
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
                columnDefs = 
                  list(
                    list(
                      targets = 2:4,
                      visible = FALSE
                    )
                  )
              )
          ) %>% 
            formatStyle(
              columns = 1:2,
              valueColumns = "color.primary",
              backgroundColor = 
                styleEqual(
                  sort(unique(teamInfo$color.primary)), 
                  sort(unique(teamInfo$color.primary))
                )
            ) %>% 
            formatStyle(
              columns = 1:2,
              valueColumns = "color.secondary",
              color = 
                styleEqual(
                  sort(unique(teamInfo$color.secondary)), 
                  sort(unique(teamInfo$color.secondary))
                )
            )
        }
      })
      
      output$leaderConceded <- renderDT({
        if(length(goalieStats()) == 0){
          NULL
        } else {
          datatable(
            goalieStats() %>% 
              select(Name, Conceded, Club) %>% 
              left_join(
                teamInfo %>% 
                  select(
                    team, 
                    color.primary,
                    color.secondary
                  ),
                by = c("Club" = "team")
              ) %>%  
              arrange(-Conceded) %>% 
              slice_head(n = 5),
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
                columnDefs = 
                  list(
                    list(
                      targets = 2:4,
                      visible = FALSE
                    )
                  )
              )
          ) %>% 
            formatStyle(
              columns = 1:2,
              valueColumns = "color.primary",
              backgroundColor = 
                styleEqual(
                  sort(unique(teamInfo$color.primary)), 
                  sort(unique(teamInfo$color.primary))
                )
            ) %>% 
            formatStyle(
              columns = 1:2,
              valueColumns = "color.secondary",
              color = 
                styleEqual(
                  sort(unique(teamInfo$color.secondary)), 
                  sort(unique(teamInfo$color.secondary))
                )
            )
        }
      })
     
    }
  )
}

