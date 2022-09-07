
###########################################################################
###########################################################################
###                                                                     ###
###                 INDIVIDUAL STATS FOR THE SSL INDEX                  ###
###                                                                     ###
###########################################################################
###########################################################################


### UI module for player similarities using MDS
playerRecordsUI <- function(id){
  
  ## Creating the namespacing function for all IDs
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 12,
          offset = 0,
          tabBox(
            width = NULL,
            ##----------------------------------------------------------------
            ##                        League Records                         -
            ##----------------------------------------------------------------
            tags$head(
              tags$style(
                HTML(
                  '.info-box {min-height: 65px;} 
                  .info-box-icon {background: transparent; height: 65px; line-height: 65px;} 
                  .info-box-content {padding-top: 0px; padding-bottom: 0px;}'
                )
              ),
              ## Imports all 5.7.2 Font Awesome Icons
              tags$style("@import url(https://use.fontawesome.com/releases/v6.0.0/css/all.css);")
            ),
            
            tabPanel(
              title = "League Records",
              column(
                width = 4,
                h3("Record for"),
                actionLink(
                  ns("topGoal"),
                  uiOutput(
                    outputId = ns("topGoal")
                  )
                ),
                actionLink(
                  ns("topAssist"),
                  uiOutput(
                    outputId = ns("topAssist")
                  )  
                ),
                actionLink(
                  ns("topxG"),
                  uiOutput(
                    outputId = ns("topxG")
                  )  
                ),
                actionLink(
                  ns("topKeyPasses"),
                  uiOutput(
                    outputId = ns("topKeyPasses")
                  )  
                ),
                actionLink(
                  ns("topChancesCreated"),
                  uiOutput(
                    outputId = ns("topChancesCreated")
                  )  
                ),
                actionLink(
                  ns("topPotM"),
                  uiOutput(
                    outputId = ns("topPotM")
                  )  
                ),
                actionLink(
                  ns("topDistanceRun"),
                  uiOutput(
                    outputId = ns("topDistanceRun")
                  )  
                ),
                actionLink(
                  ns("topYellows"),
                  uiOutput(
                    outputId = ns("topYellows")
                  )  
                ),
                actionLink(
                  ns("topReds"),
                  uiOutput(
                    outputId = ns("topReds")
                  )  
                ),
                actionLink(
                  ns("topInterceptions"),
                  uiOutput(
                    outputId = ns("topInterceptions")
                  )  
                )
              ),
              column(
                width = 8,
                h3("Top 20", align = "center"),
                reactableOutput(
                  outputId = ns("leagueRecord")
                )
              )
            ),
            tabPanel(
              title = "Cup Records",
              column(
                width = 4,
                h3("Record for"),
                actionLink(
                  ns("topGoalC"),
                  uiOutput(
                    outputId = ns("topGoalC")
                  )
                ),
                actionLink(
                  ns("topAssistC"),
                  uiOutput(
                    outputId = ns("topAssistC")
                  )  
                ),
                actionLink(
                  ns("topxGC"),
                  uiOutput(
                    outputId = ns("topxGC")
                  )  
                ),
                actionLink(
                  ns("topKeyPassesC"),
                  uiOutput(
                    outputId = ns("topKeyPassesC")
                  )  
                ),
                actionLink(
                  ns("topChancesCreatedC"),
                  uiOutput(
                    outputId = ns("topChancesCreatedC")
                  )  
                ),
                actionLink(
                  ns("topPotMC"),
                  uiOutput(
                    outputId = ns("topPotMC")
                  )  
                ),
                actionLink(
                  ns("topDistanceRunC"),
                  uiOutput(
                    outputId = ns("topDistanceRunC")
                  )  
                ),
                actionLink(
                  ns("topYellowsC"),
                  uiOutput(
                    outputId = ns("topYellowsC")
                  )  
                ),
                actionLink(
                  ns("topRedsC"),
                  uiOutput(
                    outputId = ns("topRedsC")
                  )  
                ),
                actionLink(
                  ns("topInterceptionsC"),
                  uiOutput(
                    outputId = ns("topInterceptionsC")
                  )  
                )
              ),
              column(
                width = 8,
                h3("Top 20", align = "center"),
                reactableOutput(
                  outputId = ns("cupRecord")
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
playerRecordsSERVER <- function(id){
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
      ##                        Career Records                         -
      ##----------------------------------------------------------------
      
      output$topGoal <- renderUI({
        
        leader <- 
          leagueRecords() %>% 
          select(
            Name, 
            Stat = Goals,
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Goals")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-futbol", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "goals.",
                sep = " "
                ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topAssist <- renderUI({
        
        leader <- 
          leagueRecords() %>% 
          select(
            Name, 
            Stat = Assists,
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Assists")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-shoe-prints", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "assists.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topxG <- renderUI({
        
        leader <- 
          leagueRecords() %>% 
          select(
            Name, 
            Stat = xG,
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Expected Goals")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-futbol", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "expected goals.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topKeyPasses <- renderUI({
        
        leader <- 
          leagueRecords() %>% 
          select(
            Name, 
            Stat = "`Key Passes`",
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Key Passes")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-key", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "passes.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topChancesCreated <- renderUI({
        
        leader <- 
          leagueRecords() %>% 
          select(
            Name, 
            Stat = "`Chances Created`",
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Chances Created")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-eye", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "chances.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topInterceptions <- renderUI({
        
        leader <- 
          leagueRecords() %>% 
          select(
            Name, 
            Stat = "Interceptions",
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Interceptions")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-ban", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "interceptions.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topPotM <- renderUI({
        
        leader <- 
          leagueRecords() %>% 
          select(
            Name, 
            Stat = "`Player of the Match`",
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Player of the Match")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-award", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "awards.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topYellows <- renderUI({
        
        leader <- 
          leagueRecords() %>% 
          select(
            Name, 
            Stat = "`Yellow Cards`",
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Yellow Cards")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-square", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "yellow.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topReds <- renderUI({
        
        leader <- 
          leagueRecords() %>% 
          select(
            Name, 
            Stat = "`Red Cards`",
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Red Cards")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-exclamation", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "red.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topDistanceRun <- renderUI({
        
        leader <- 
          leagueRecords() %>% 
          select(
            Name, 
            Stat = "`Distance Run (km)`",
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Distance Run (km)")
            ),
          color = "navy",
          width = NULL,
          icon = icon("person-running", style="font-size: 36px; color: white", verify_fa = FALSE),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "km.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      #Penalties Scored, Key Headers, Key Tackles
      
      output$topGoalC <- renderUI({
        
        leader <- 
          cupRecords() %>% 
          select(
            Name, 
            Stat = Goals,
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Goals")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-futbol", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "goals.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topAssistC <- renderUI({
        
        leader <- 
          cupRecords() %>% 
          select(
            Name, 
            Stat = Assists,
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Assists")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-shoe-prints", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "assists.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topxGC <- renderUI({
        
        leader <- 
          cupRecords() %>% 
          select(
            Name, 
            Stat = xG,
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Expected Goals")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-futbol", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "expected goals.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topKeyPassesC <- renderUI({
        
        leader <- 
          cupRecords() %>% 
          select(
            Name, 
            Stat = "`Key Passes`",
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Key Passes")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-key", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "passes.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topChancesCreatedC <- renderUI({
        
        leader <- 
          cupRecords() %>% 
          select(
            Name, 
            Stat = "`Chances Created`",
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Chances Created")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-eye", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "chances.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topInterceptionsC <- renderUI({
        
        leader <- 
          cupRecords() %>% 
          select(
            Name, 
            Stat = "Interceptions",
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Interceptions")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-ban", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "interceptions.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topPotMC <- renderUI({
        
        leader <- 
          cupRecords() %>% 
          select(
            Name, 
            Stat = "`Player of the Match`",
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Player of the Match")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-award", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "awards.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topYellowsC <- renderUI({
        
        leader <- 
          cupRecords() %>% 
          select(
            Name, 
            Stat = "`Yellow Cards`",
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Yellow Cards")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-square", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "yellow.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topRedsC <- renderUI({
        
        leader <- 
          cupRecords() %>% 
          select(
            Name, 
            Stat = "`Red Cards`",
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Red Cards")
            ),
          color = "navy",
          width = NULL,
          icon = tags$i(class = "fas fa-exclamation", style="font-size: 36px; color: white"),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "red.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })
      
      output$topDistanceRunC <- renderUI({
        
        leader <- 
          cupRecords() %>% 
          select(
            Name, 
            Stat = "`Distance Run (km)`",
          ) %>% 
          arrange(
            desc(Stat)
          ) %>% 
          collect() %>% 
          slice_head(n = 1)
        
        infoBox(
          title = 
            tags$b(
              paste(
                "Distance Run (km)")
            ),
          color = "navy",
          width = NULL,
          icon = icon("person-running", style="font-size: 36px; color: white", verify_fa = FALSE),
          fill = TRUE,
          value = 
            tags$p(
              paste(
                leader$Name, "with", leader$Stat, "km.",
                sep = " "
              ),
              style = "font-size: 75%;"
            )
        ) 
        
      })

      ##---------------------------------------------------------------
      ##                    Selected stat observers                   -
      ##---------------------------------------------------------------
      
      currentStat <- 
        reactiveValues(
          league = "Goals",
          cup = "Goals"
        )
      
      ##---------
      ##  League  
      ##---------
      
      observeEvent(
        input$topGoal, 
        {
          currentStat$league <- "Goals"
        }
      )
      
      observeEvent(
        input$topPotM, 
        {
          currentStat$league <- "`Player of the Match`"
        }
      )
      
      observeEvent(
        input$topInterceptions, 
        {
          currentStat$league <- "Interceptions"
        }
      )
      
      observeEvent(
        input$topChancesCreated, 
        {
          currentStat$league <- "`Chances Created`"
        }
      )
      
      observeEvent(
        input$topDistanceRun, 
        {
          currentStat$league <- "`Distance Run (km)`"
        }
      )
      
      observeEvent(
        input$topYellows, 
        {
          currentStat$league <- "`Yellow Cards`"
        }
      )
      
      observeEvent(
        input$topReds, 
        {
          currentStat$league <- "`Red Cards`"
        }
      )
      
      observeEvent(
        input$topAssist, 
        {
          currentStat$league <- "Assists"
        }
      )
      
      observeEvent(
        input$topxG, 
        {
          currentStat$league <- "xG"
        }
      )
      
      observeEvent(
        input$topKeyPasses, 
        {
          currentStat$league <- "`Key Passes`"
        }
      )
      
      ##-------
      ##  Cup  
      ##-------

      observeEvent(
        input$topGoalC, 
        {
          currentStat$cup <- "Goals"
        }
      )
      
      observeEvent(
        input$topPotMC, 
        {
          currentStat$cup <- "`Player of the Match`"
        }
      )
      
      observeEvent(
        input$topInterceptionsC, 
        {
          currentStat$cup <- "Interceptions"
        }
      )
      
      observeEvent(
        input$topChancesCreatedC, 
        {
          currentStat$cup <- "`Chances Created`"
        }
      )
      
      observeEvent(
        input$topDistanceRunC, 
        {
          currentStat$cup <- "`Distance Run (km)`"
        }
      )
      
      observeEvent(
        input$topYellowsC, 
        {
          currentStat$cup <- "`Yellow Cards`"
        }
      )
      
      observeEvent(
        input$topRedsC, 
        {
          currentStat$cup <- "`Red Cards`"
        }
      )
      
      observeEvent(
        input$topAssistC, 
        {
          currentStat$cup <- "Assists"
        }
      )
      
      observeEvent(
        input$topxGC, 
        {
          currentStat$cup <- "xG"
        }
      )
      
      observeEvent(
        input$topKeyPassesC, 
        {
          currentStat$cup <- "`Key Passes`"
        }
      )
      
      
      ##--------
      ##  Data  
      ##--------
      
      leagueRecords <- reactive({
        tbl(con, "gameDataPlayer") %>% 
          filter(
            !(Matchday %like% "%Cup%")
          ) %>% 
          group_by(Name) %>% 
          summarize(
            across(
              c(Apps, 
                Goals, 
                Assists, 
                xG, 
                `Key Passes`,
                `Key Headers`, 
                `Key Tackles`,
                `Chances Created`,
                `Interceptions`,
                `Distance Run (km)`,
                `Penalties Scored`,
                `Yellow Cards`,
                `Red Cards`,
                `Player of the Match`
                ),
              sum
            )
          ) %>% 
          collect()
      })
      
      output$leagueRecord <- renderReactable({
          leagueRecords() %>% 
          select(
            Name, 
            Apps,
            currentStat$league
          ) %>% 
          arrange(
            across(
              currentStat$league,
              desc
            )
          ) %>% 
          slice_head(n = 20) %>% 
          mutate(
            Rank = 1:n()
          ) %>% 
          relocate(
            Rank,
            .before = Name
          ) %>% 
          reactable(
            pagination = FALSE,
            theme = reactableTheme(
              backgroundColor = "#F8F8F8"
            ),
            defaultColDef = 
              colDef(
                maxWidth = 150
              ),
            columns = 
              list(
                Rank = 
                  colDef(
                    width = 60
                  ),
                Name = 
                  colDef(
                    maxWidth = 350
                  )
              )
          )
      })
      
      cupRecords <- reactive({
        tbl(con, "gameDataPlayer") %>% 
          filter(
            (Matchday %like% "%Cup%")
          ) %>% 
          group_by(Name) %>% 
          summarize(
            across(
              c(Apps, 
                Goals, 
                Assists, 
                xG, 
                `Key Passes`,
                `Key Headers`, 
                `Key Tackles`,
                `Chances Created`,
                `Interceptions`,
                `Distance Run (km)`,
                `Penalties Scored`,
                `Yellow Cards`,
                `Red Cards`,
                `Player of the Match`
              ),
              sum
            )
          ) %>% 
          collect()
      })
      
      output$cupRecord <- renderReactable({
        cupRecords() %>% 
          select(
            Name, 
            Apps,
            currentStat$cup
          ) %>% 
          arrange(
            across(
              currentStat$cup,
              desc
            )
          ) %>% 
          slice_head(n = 20) %>% 
          mutate(
            Rank = 1:n()
          ) %>% 
          relocate(
            Rank,
            .before = Name
          ) %>% 
          reactable(
            pagination = FALSE,
            theme = reactableTheme(
              backgroundColor = "#F8F8F8"
            ),
            defaultColDef = 
              colDef(
                maxWidth = 150
              ),
            columns = 
              list(
                Rank = 
                  colDef(
                    width = 60
                  ),
                Name = 
                  colDef(
                    maxWidth = 350
                  )
              )
          )
      })
      
      
     
    }
  )
}

