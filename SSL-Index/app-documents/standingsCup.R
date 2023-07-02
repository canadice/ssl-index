standingsCupUI <- function(id) {
  ns <- NS(id)
  tagList(
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
    uiOutput(outputId = ns("boxes")) %>% 
      div()
  )
}

standingsCupServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      create_box <- 
        function(
          boxData,  
          width = 12, 
          background = 'green',
          solid_header = TRUE
        ) {
          boxId <- paste0('box', boxData[1])
          subtitleId <- paste0('box', boxData[1], "_sub")
          
          if(!(boxData[5] %>% is.na())){
            divs <- 
              tagList(
                boxData[5] %>% 
                  str_split("-", simplify = TRUE) %>% 
                  lapply(
                    X = .,
                    FUN = function(x){
                      if(x != "NA"){
                        if((x %>% str_extract("[0-9]+") %>% is.na() %>% suppressWarnings())){
                          if(x %>% str_detect("Team")){
                            
                          } else {
                            logo <- 
                              img(
                                class = "logo",
                                src = sprintf("%s.png", x),
                                style = "height: 30px;", 
                                alt = x
                              )
                            
                            div(class = "cup-info-div", logo, x)                            
                          }
                        } else {
                          div(class = "cup-info-div scores", x)
                        }
                      } 
                    }
                  )
              )

            box(id = boxId,
                divs,
                width = width,
                solidHeader = solid_header
            )
          } else {
            div()
            # box(
            #   id = boxId,
            #   background = "black"
            # )
          }
          
        }
      
      currentCupSchedule <- reactive({
          read_sheet(
            ss = "https://docs.google.com/spreadsheets/d/1jcsFLjtiq-jK273DI-m-N38x9yUS66HwuX5x5Uig8Uc/edit?usp=sharing", 
            sheet = paste("Season", input$season)
          ) %>% 
          mutate(
            across(
              Division:Matchday,
              unlist
            )
          ) %>% 
          filter(
            Division == "Cup"
          ) %>% 
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
          ) 
          
      })
      
      output$boxes <- renderUI({
        ncol <- 3
        nrBoxes <- ncol*8
        
        scheduleData <- 
          currentCupSchedule() %>%
          # temp %>%
          filter(str_detect(Matchday, pattern = "Final")) %>% 
          group_by(Home, Away) %>% 
          mutate(
            Matchup = 
              c(Home, Away) %>% 
              unique() %>% 
              sort() %>% 
              paste0(collapse = " - "),
            Leg = 
              case_when(
                Matchday != "Final" ~ 
                  Matchday %>% 
                    str_extract_all(pattern = "Leg [12]"),
                TRUE ~ list("Leg 1")
              ) %>% unlist(),
            Matchday = 
              Matchday %>% 
              str_extract_all(pattern = "[A-z ]*Final", simplify = TRUE)
          ) %>% 
          select(Matchday, Leg, Matchup, HomeScore, AwayScore) %>% 
          pivot_longer(
            cols = c(HomeScore, AwayScore)
          ) %>% 
          mutate(
            Team = 
              case_when(
                name == "AwayScore" ~ Away,
                TRUE ~ Home
              )
          ) %>% 
          ungroup() %>% 
          select(-Home, -Away) %>% 
          pivot_wider(
            id_cols = c(Matchup, name, Matchday),
            names_from = c(Leg),
            values_from = c(value, Team),
            names_vary = "slowest"
          ) %>% 
          group_by(Matchday, Matchup) %>% 
          summarize(
            text = 
              paste(
                `Team_Leg 1`,
                `value_Leg 2` %>% rev(), 
                `value_Leg 1`,
                sep = "-"
              ) %>% 
              paste0(collapse = " | ")
          ) %>% 
          group_by(Matchday, Matchup) %>% 
          mutate(
            top = text %>% 
              str_split("\\|", simplify = TRUE) %>% 
              str_trim() %>% 
              .[1],
            bottom = text %>% 
              str_split("\\|", simplify = TRUE) %>% 
              str_trim() %>% 
              .[2]
          ) %>% 
          select(
            -text
          ) %>% 
          pivot_longer(
            cols = c(top, bottom)
          ) %>% 
          mutate(
            Matchday = 
              factor(
                Matchday,
                levels = c("Quarter Final", "Semi Final", "Final")
              )
          ) %>% 
          arrange(
            Matchday
          ) %>% 
          ungroup() %>% 
          mutate(
            boxInfo = 
              c(
                seq(1, nrBoxes, ncol),
                seq(ncol*2+2, ncol*5+2, ncol),
                seq(ncol*3+3, ncol*4+3, ncol)
              )
          )
        
        data <- 
          data.frame(
            id = 1:nrBoxes
          ) %>% 
          left_join(
            scheduleData,
            by = c("id" = "boxInfo")
          )
        
        apply(
          X = data, 
          MARGIN = 1,
          FUN = create_box
        )
      })
      
      
    }
  )
}
