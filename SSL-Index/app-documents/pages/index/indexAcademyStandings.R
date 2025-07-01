
############################################################################
############################################################################
###                                                                      ###
###                  LEAGUE STANDINGS FOR THE SSL INDEX                  ###
###                                                                      ###
############################################################################
############################################################################


### UI module for player similarities using MDS
academyStandingsUI <- function(id){
  
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
            choices = 21
          )
        )
      ),
      fluidRow(
        column(
          width = 10,
          offset = 1,
          h4("Academy", align = "center"),
          reactableOutput(
            outputId = ns("standings1")
            ) %>% 
            withSpinner()
        )
      )
    )
 )
}

## Backend module for player similarities
academyStandingsSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){
      
      standings1 <- 
        reactive({
          sheet <- 
            read_sheet(
              ss = "https://docs.google.com/spreadsheets/d/1jcsFLjtiq-jK273DI-m-N38x9yUS66HwuX5x5Uig8Uc/edit?usp=sharing", 
              sheet = paste("Season", input$season, "Academy")
            ) %>% 
            mutate(
              across(
                Division:Matchday,
                unlist
              )
            )
          
          sheet %>% 
            mutate(
              `In-game Date` = `In-game Date` %>% as_date() %>% format(format = "%m/%d"),
              `IRL Date` = `IRL Date` %>% as_date() %>% format(format = "%m/%d"),
              HomeScore = 
                stringr::str_split(
                  Result, 
                  pattern = "-", 
                  simplify = TRUE
                )[,1], 
              AwayScore = 
                stringr::str_split(
                  Result, 
                  pattern = "-", 
                  simplify = TRUE
                )[,2]
            ) %>%
            mutate(
              HomePoints = 
                case_when(
                  str_detect(HomeScore, "e|p") ~ 3,
                  HomeScore %>% as.numeric() > AwayScore %>% as.numeric() ~ 3,
                  HomeScore %>% as.numeric() == AwayScore %>% as.numeric() ~ 1,
                  TRUE ~ 0
                ),
              AwayPoints = 
                case_when(
                  str_detect(AwayScore, "e|p") ~ 3,
                  HomeScore %>% as.numeric() < AwayScore %>% as.numeric() ~ 3,
                  HomeScore %>% as.numeric() == AwayScore %>% as.numeric() ~ 1,
                  TRUE ~ 0
                )
            ) %>% 
            select(
              -`IRL Date`,
              -`In-game Date`,
              -Result
            ) %>% 
            pivot_longer(
              c(HomePoints, AwayPoints),
              names_to = c("set", ".value"),
              names_pattern = "(....)(.*)$"
            ) %>% 
            mutate(
              Team = 
                case_when(
                  set == "Home" ~ Home,
                  TRUE ~ Away
                ),
              GF = 
                case_when(
                  set == "Home" ~ HomeScore,
                  TRUE ~ AwayScore
                ),
              GA = 
                case_when(
                  set == "Home" ~ AwayScore,
                  TRUE ~ HomeScore
                ),
              Matchday = unlist(Matchday)
            ) %>% 
            select(
              -contains("Home"),
              -contains("Away"),
              -set
            ) %>% 
            filter(
              !is.na(GF) & GF != "",
              !is.na(as.numeric(Matchday) %>% suppressWarnings()) 
            ) %>% 
            group_by(
              Team
            ) %>% 
            summarize(
              GP = n(),
              W = sum(Points == 3),
              D = sum(Points == 1),
              L = sum(Points == 0),
              GF = sum(as.numeric(GF)),
              GA = sum(as.numeric(GA)),
              GD = GF-GA,
              Points = sum(Points)
            ) %>% 
            arrange(
              desc(Points),
              desc(GD)
            ) %>% 
            ungroup() %>% 
            mutate(
              Pos = 1:n()
            ) |> 
            relocate(
              c(Pos), 
              .before = Team
            ) %>% 
            rename(
              Pts = Points
            )
        })
      
      output$standings1 <- renderReactable({
        reactable(
          standings1(), 
          pagination = FALSE,
          fullWidth = FALSE,
          defaultColDef = colDef(minWidth = 60),
          columns = 
            list(
              Team = colDef(name = "", width = 200, align = "left", cell = function(value){
                image <- img(src = sprintf("%s.png", value), style = "height: 30px;", alt = value, title = value)  
                
                list <- 
                  tagList(
                    flexRow(style = "align-items: center; gap: 8px;", tagList(
                      image,
                      span(class = "truncated-text", value)
                    ))
                  )
              }),
              GP = colDef(header = tippy("GP", "Games played", placement = "top", theme = "ssl", arrow = TRUE)),
              W = colDef(header = tippy("W", "Wins", placement = "top", theme = "ssl", arrow = TRUE)),
              D = colDef(header = tippy("D", "Draws", placement = "top", theme = "ssl", arrow = TRUE)),
              L = colDef(header = tippy("L", "Losses", placement = "top", theme = "ssl", arrow = TRUE)),
              GF = colDef(header = tippy("GF", "Goals scored", placement = "top", theme = "ssl", arrow = TRUE)),
              GA = colDef(header = tippy("GA", "Goals conceded", placement = "top", theme = "ssl", arrow = TRUE)),
              Pts = colDef(header = tippy("P", "Points", placement = "top", theme = "ssl", arrow = TRUE))
            )
        ) 
      })
      
      
    }
  )
}

