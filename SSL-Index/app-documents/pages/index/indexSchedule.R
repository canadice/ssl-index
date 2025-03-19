leagueScheduleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      ## First row
      fluidRow(
        column(
          width = 4,
          selectInput(
            inputId = ns("selectedSeason"),
            label = "Select a season",
            choices = 
              c(
                1:currentSeason$season |> 
                sort(decreasing = TRUE)
              )
          )
        ),
        column(
          width = 6
        ),
        column(
          width = 2,
          uiOutput(ns("leagueSelector"))
        )
      ),
      ## Second row
      fluidRow(
        h2("Schedule"),
        uiOutput(ns("schedule"))
      )
    ) # close fluidpage
  ) # close tagList
}

leagueScheduleServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #### DATA GENERATION ####
      schedule <- reactive({
        req(input$selectedLeague)
        
        readAPI(url = "https://api.simulationsoccer.com/index/schedule", 
                query = list(league = input$selectedLeague, season = input$selectedSeason)
        )
      })
      
      
      #### UI OUTPUT ####
      output$leagueSelector <- renderUI({
        season <- input$selectedSeason |> as.numeric()
        
        if(season < 5){
          selectInput(
            inputId = session$ns("selectedLeague"),
            label = "League",
            choices = 
              c(
                "ALL",
                "League" = "1",
                "Cup" = "0"
              )
          )
        } else if (season %in% c(12, 18)){
          selectInput(
            inputId = session$ns("selectedLeague"),
            label = "League",
            choices = 
              c(
                "ALL",
                "Major" = "1",
                "Minor" = "2",
                "Cup" = "0",
                "WSFC" = "5"
              )
          )
        } else if (season < 12){
          selectInput(
            inputId = session$ns("selectedLeague"),
            label = "League",
            choices = 
              c(
                "ALL",
                "Division 1" = "1",
                "Division 2" = "2",
                "Cup" = "0"
              )
          )
        } else {
          selectInput(
            inputId = session$ns("selectedLeague"),
            label = "League",
            choices = 
              c(
                "ALL",
                "Major" = "1",
                "Minor" = "2",
                "Cup" = "0"
              )
          )
        }
      })
      
      output$schedule <- renderUI({
        season <- input$selectedSeason
        league <- input$selectedLeague
        
        if(schedule() |> is_empty()){
          NULL 
        } else {
          schedule() |> 
            {
              if (!"HomeScore" %in% names(.)) mutate(., HomeScore = NA_character_) else .
            } |>
            {
              if (!"AwayScore" %in% names(.)) mutate(., AwayScore = NA_character_) else .
            } |>
            {
              if (!"Penalties" %in% names(.)) mutate(., Penalties = NA_character_) else .
            } |>
            {
              if (!"ExtraTime" %in% names(.)) mutate(., ExtraTime = NA_character_) else .
            } |>
            rename(
              Date = IRLDate
            ) |> 
            mutate(
              across(
                c(HomeScore, AwayScore),
                function(x) ifelse(is.na(x), " ", x)
              ),
              Score = case_when(
                Penalties == 1 & HomeScore > AwayScore ~ paste0("p", paste(HomeScore, AwayScore, sep = " - ")),
                Penalties == 1 & HomeScore < AwayScore ~ paste0(paste(HomeScore, AwayScore, sep = " - "), "p"),
                ExtraTime == 1 & HomeScore > AwayScore ~ paste0("e", paste(HomeScore, AwayScore, sep = " - ")),
                ExtraTime == 1 & HomeScore < AwayScore ~ paste0(paste(HomeScore, AwayScore, sep = " - "), "p"),
                TRUE ~ paste(HomeScore, AwayScore, sep = " - ")
              ),
              MatchType = case_when(
                MatchType == -1 ~ "Friendlies",
                MatchType == 0 ~ "Cup",
                MatchType == 1 ~ "Major League",
                MatchType == 5 ~ "WSFC",
                TRUE ~ "Minor League"
              )
            ) |> 
            select(!c(HomeScore, AwayScore, ExtraTime, Penalties)) |> 
            reactable(
              pagination = FALSE,
              searchable = TRUE,
              columns = 
                list(
                  Date = colDef(width = 100),
                  MatchType = colDef(width = 100),
                  MatchDay = colDef(width = 100),
                  Home = 
                    colDef(
                      cell = function(value){
                        image <- img(src = sprintf("%s.png", value), style = "height: 30px;", title = value)
                        
                        tagList(
                          div(style = "display: inline-block; width: 30px;", image),
                          div(style = "font-size: 1.2rem", value)
                        )
                      }
                    ),
                  Away = 
                    colDef(
                      cell = function(value){
                        image <- img(src = sprintf("%s.png", value), style = "height: 30px;", title = value)
                        
                        tagList(
                          div(style = "display: inline-block; width: 30px;", image),
                          div(style = "font-size: 1.2rem", value)
                        )
                      }
                    )
                )
            )
        }
      })
      
    }
  )
}
