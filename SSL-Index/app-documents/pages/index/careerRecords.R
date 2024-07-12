careerRecordsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      ## First row
      fluidRow(
        column(
          width = 4,
          selectInput(
            inputId = ns("selectedLeague"),
            label = "League",
            choices = 
              c(
                "ALL",
                "Major" = "1",
                "Minor" = "2",
                "Cup",
                "WSFC"
              )
          )
        )
      ),
      ## Second row
      fluidRow(
        tabsetPanel(
          width = NULL,
          header = 
            tags$head(
              tags$style(
                HTML(
                  '.info-box {min-height: 65px;} 
                    .info-box-icon {background: transparent; height: 65px; line-height: 65px;} 
                    .info-box-content {padding-top: 0px; padding-bottom: 0px;}'
                )
              ),
              ## Imports all 6.0.0 Font Awesome Icons
              tags$style("@import url(https://use.fontawesome.com/releases/v6.0.0/css/all.css);")
            ),
          tabPanel(
            title = "Outfield Records",
            column(
              width = 4,
              h3("Record for"),
              uiOutput(ns("recordList")) %>% 
                withSpinnerMedium()
            ),
            column(
              width = 8,
              h3("Top 20", align = "center"),
              reactableOutput(
                outputId = ns("leagueRecord")
              ) %>% 
                withSpinnerMedium()
            )
          ),
          tabPanel(
            title = "Keeper Records",
            column(
              width = 4,
              h3("Record for"),
              uiOutput(ns("recordListKeeper"))
            ),
            column(
              width = 8,
              h3("Top 20", align = "center"),
              reactableOutput(
                outputId = ns("leagueRecordKeeper")
              )
            )  
          )
        )
      )
    ) # close fluidpage
  ) # close tagList
}

careerRecordsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #### DATA GENERATION ####
      outfieldData <- reactive({
        req(input$selectedLeague)
        getOutfieldCareer(league = input$selectedLeague) 
      })
      
      keeperData <- reactive({
        req(input$selectedLeague)
        getKeeperCareer(league = input$selectedLeague) 
      })
      
      currentStatistic <- reactiveVal("goals")
      currentStatisticKeeper <- reactiveVal("won")
      
      #### UI OUTPUT ####
      outstatistics <- c("goals", "assists", "xg", "distance run (km)", "key passes", "chances created", "tackles won", "interceptions", "yellow cards", "red cards")
      
      keepstatistics <- c("won", "clean sheets", "conceded", "save%")
      
      output$recordList <- renderUI({
        lapply(outstatistics, function(stat) {
          actionLink(
            session$ns(paste0(stat, "_record_click")),
            uiOutput(session$ns(paste0(stat, "_record")))
          )
        })
      })
      
      output$recordListKeeper <- renderUI({
        lapply(keepstatistics, function(stat) {
          actionLink(
            session$ns(paste0(stat, "_record_click")),
            uiOutput(session$ns(paste0(stat, "_record")))
          )
        })
      })
      
      ##
      lapply(outstatistics, function(stat){
        output[[paste0(stat, "_record")]] <- renderUI({
          outfieldData() %>% 
            then(
              onFulfilled = function(data){
                leader <- 
                  data %>% 
                  select(
                    name, club, !!sym(stat)
                  ) %>% 
                  arrange(!!sym(stat) %>% desc()) %>% 
                  slice_head(n = 1)
                
                infoBox(
                  title = 
                    tags$b(paste(stat %>% str_to_title())),
                  color = "light-blue",
                  width = NULL,
                  icon = tags$i(class = "fas fa-exclamation", style="font-size: 36px; color: white"),
                  fill = TRUE,
                  value = tags$p(paste(leader$name, "with", leader[,stat], stat, sep = " "), style = "font-size: 75%;")
                ) 
              }
            )
        })
      })
      
      lapply(keepstatistics, function(stat){
        output[[paste0(stat, "_record")]] <- renderUI({
          keeperData() %>% 
            then(
              onFulfilled = function(data){
                leader <- 
                  data %>% 
                  select(
                    name, club, !!sym(stat)
                  ) %>% 
                  arrange(!!sym(stat) %>% desc()) %>% 
                  slice_head(n = 1)
                
                infoBox(
                  title = 
                    tags$b(paste(stat %>% str_to_title())),
                  color = "light-blue",
                  width = NULL,
                  icon = tags$i(class = "fas fa-exclamation", style="font-size: 36px; color: white"),
                  fill = TRUE,
                  value = tags$p(paste(leader$name, "with", leader[,stat], stat, sep = " "), style = "font-size: 75%;")
                ) 
              }
            )
        })
      })
      
      
      ## Observers to change the selected statistic for the top 20
      lapply(outstatistics, function(stat){
        observe(
          currentStatistic(stat)
        ) %>% 
          bindEvent(
            input[[paste0(stat, "_record_click")]]
          )
      })
      
      lapply(keepstatistics, function(stat){
        observe(
          currentStatisticKeeper(stat)
        ) %>% 
          bindEvent(
            input[[paste0(stat, "_record_click")]]
          )
      })
      
      #### REACTABLE OUTPUT ####
      output$leagueRecord <- renderReactable({
        outfieldData() %>% 
          then(
            onFulfilled = function(data){
              stat <- currentStatistic()
              
              data %>% 
                select(
                  name, club, !!sym(stat)
                ) %>% 
                arrange(!!sym(stat) %>% desc()) %>% 
                slice_head(n = 20) %>% 
                mutate(
                  RANK = 1:n()
                ) %>% 
                relocate(RANK) %>% 
                recordReactable()
            }
          )
      })
      
      output$leagueRecordKeeper <- renderReactable({
        keeperData() %>% 
          then(
            onFulfilled = function(data){
              stat <- currentStatisticKeeper()
              
              data %>% 
                select(
                  name, club, !!sym(stat)
                ) %>% 
                arrange(!!sym(stat) %>% desc()) %>% 
                slice_head(n = 20) %>% 
                mutate(
                  RANK = 1:n()
                ) %>% 
                relocate(RANK) %>% 
                recordReactable()
            }
          )
      })
      
      
    }
  )
}