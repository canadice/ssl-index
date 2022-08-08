
############################################################################
############################################################################
###                                                                      ###
###                  LEAGUE STANDINGS FOR THE SSL INDEX                  ###
###                                                                      ###
############################################################################
############################################################################


### UI module for player similarities using MDS
standingsUI <- function(id){
  
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
              1:max(playerGameData$Season) %>% 
              sort(decreasing = TRUE)
          )
        )
      ),
      fluidRow(
        column(
          width = 10,
          offset = 1,
          reactableOutput(outputId = ns("standings1")),
          br(),
          uiOutput(
            outputId = ns("standings2UI")
          )
        )
      )
    )
 )
}

## Backend module for player similarities
standingsSERVER <- function(id){
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
              sheet = paste("Season", input$season)
            ) 
          
          if(input$season > 4){
            sheet <- 
              sheet %>% 
              filter(
                Division == 1
              ) 
          }
            
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
                  HomeScore > AwayScore ~ 3,
                  HomeScore == AwayScore ~ 1,
                  TRUE ~ 0
                ),
              AwayPoints = 
                case_when(
                  str_detect(AwayScore, "e|p") ~ 3,
                  HomeScore < AwayScore ~ 3,
                  HomeScore == AwayScore ~ 1,
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
            ) %>% 
            left_join(
              teamInfo %>% 
                select(
                  team, 
                  color_primary,
                  color_secondary,
                  logo
                ),
              by = c("Team" = "team")
            ) %>% 
            relocate(
              c(Pos, logo), 
              .before = Team
            ) %>% 
            rename(
              ` ` = logo,
              Pts = Points
            )
        })
      
      standings2 <- 
        reactive({
          sheet <- 
            read_sheet(
              ss = "https://docs.google.com/spreadsheets/d/1jcsFLjtiq-jK273DI-m-N38x9yUS66HwuX5x5Uig8Uc/edit?usp=sharing", 
              sheet = paste("Season", input$season)
            ) 
          
          if(input$season > 4){
            sheet <- 
              sheet %>% 
              filter(
                Division == 2
              ) 
            
            if(all(sheet$Result %>% is.na())){
              sheet <- NULL
            } else {
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
                    HomeScore > AwayScore ~ 3,
                    HomeScore == AwayScore ~ 1,
                    TRUE ~ 0
                  ),
                  AwayPoints = 
                  case_when(
                    str_detect(AwayScore, "e|p") ~ 3,
                    HomeScore < AwayScore ~ 3,
                    HomeScore == AwayScore ~ 1,
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
                ) %>% 
                left_join(
                  teamInfo %>% 
                  select(
                    team, 
                    color_primary,
                    color_secondary,
                    logo
                  ),
                  by = c("Team" = "team")
                ) %>% 
                relocate(
                  c(Pos, logo), 
                  .before = Team
                ) %>% 
                rename(
                  ` ` = logo,
                  Pts = Points
                )
            } 
          } else {
            sheet <- NULL
          }
        })
      
      output$standings1 <- renderReactable({
        reactable(
          standings1() %>% 
            select(
              -contains("color")
            ), 
          pagination = FALSE,
          fullWidth = FALSE,
          defaultColDef = 
            colDef(
              maxWidth = 60,
              align = "center",
              style = function(value, index){
                list(
                  background = 
                    standings1()$color_primary[index],
                  color = 
                    standings1()$color_secondary[index]
                  )
              }
            ),
          columns = 
            list(
              ` ` = 
                colDef(
                  width = 50,
                  align = "center",
                  class = "cell",
                  cell = function(value){
                    logo <- 
                      img(
                        class = "logo",
                        src = value,
                        height = 30
                      )
                    
                    div(class = "club", logo)
                  }
                ),
              Team = 
                colDef(
                  width = 150,
                  align = "left",
                  style = 
                    function(value, index){
                      list(
                        background = 
                          standings1()$color_primary[index],
                        color = 
                          standings1()$color_secondary[index],
                        fontWeight = "bold"
                      )
                    }
                )
            )
        ) 
      })
      
      
      output$standings2UI <- renderUI({
        if(is.null(standings2())){
          NULL
        } else {
          reactableOutput(outputId = session$ns("standings2"))
        }
      })
      
      output$standings2 <- renderReactable({
        reactable(
          standings2() %>% 
            select(
              -contains("color")
            ),
          pagination = FALSE,
          fullWidth = FALSE,
          defaultColDef = 
            colDef(
              maxWidth = 60,
              align = "center",
              style = function(value, index){
                list(
                  background = 
                    standings2()$color_primary[index],
                  color = 
                    standings2()$color_secondary[index]
                )
              }
            ),
          columns = 
            list(
              ` ` = 
                colDef(
                  width = 50,
                  align = "center",
                  class = "cell",
                  cell = function(value){
                    logo <- 
                      img(
                        class = "logo",
                        src = value,
                        height = 30
                      )
                    
                    div(class = "club", logo)
                  }
                ),
              Team = 
                colDef(
                  width = 150,
                  align = "left",
                  style = 
                    function(value, index){
                      list(
                        background = 
                          standings2()$color_primary[index],
                        color = 
                          standings2()$color_secondary[index],
                        fontWeight = "bold"
                      )
                    }
                )
            )
        ) 
      })
    }
  )
}

