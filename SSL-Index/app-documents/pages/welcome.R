welcomeUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    box(title = "Latest Results", width = NULL,
        uiOutput(ns("schedule")) %>% 
          withSpinnerSmall()
        ) %>% 
      column(width = 12),
    
    box(title = "News", width = NULL,
        column(
          width = 6,
          h4("Weekly top earners"),
          reactableOutput(ns("weeklyLeaders")) %>% 
            withSpinnerMedium()
        ),
        column(
          width = 6,
          h4("Recent creates"),
          reactableOutput(ns("created")) %>% 
            withSpinnerMedium()
        )
      ) %>% 
      column(width = 8),
    
    box(title = "Current Standings", width = NULL,
        h5("Major League"),
        reactableOutput(ns("standings_1")) %>% 
          withSpinnerSmall(),
        h5("Minor League"),
        reactableOutput(ns("standings_2")) %>% 
          withSpinnerSmall()
        ) %>% 
      column(width = 4)
    
  )
}

welcomeServer <- function(id, userinfo) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #### Latest league standings ####
      lapply(1:2,
             FUN = function(division){
               output[[paste0("standings_", division)]] <- renderReactable({
                 getStandings(division, season = currentSeason$season) %>% 
                   select(
                     Team, 
                     Wins:Losses,
                     Points
                   ) %>% 
                   reactable(
                     defaultColDef = colDef(minWidth = 50),
                     columns = 
                       list(
                         Team = colDef(
                           # width = 25,
                           cell = function(value){
                             image <- img(src = sprintf("%s.png", value), style = "height: 25px;", alt = value)  
                             
                             list <- 
                               tagList(
                                 div(style = "display: inline-block; width: 25px;", image)
                               )
                           }
                         )
                       )
                   )
               })
             })
      
      
      #### Latest results ####
      output$schedule <- renderUI({
        schedule <- getSchedule()
        
        tagList(
          div(
            class = "results",
            id = "results-scroll",
            lapply(1:nrow(schedule),
                   function(i){
                     box(
                       title = div(
                         div(style = "display: inline-block; width: 40px;", img(src = sprintf("%s.png", schedule[i, "Home"]), style = "height: 40px;", alt = schedule[i, "Home"])), 
                         strong(" - "), 
                         div(style = "display: inline-block; width: 40px;", img(src = sprintf("%s.png", schedule[i, "Away"]), style = "height: 40px;", alt = schedule[i, "Away"])),
                         align = "center"
                       ),
                       width = NULL,
                       status = "primary",
                       h4(paste(schedule[i, "HomeScore"], schedule[i, "AwayScore"], sep = "-")),
                       footer = 
                         paste(
                           if_else(schedule[i, "MatchType"] == 0, 
                                   "Cup",
                                   if_else(schedule[i, "MatchType"] == 1, 
                                           "Major League",
                                           if_else(schedule[i, "MatchType"] == 2, "Minor League", "Pre-Season"))),
                           schedule[i, "MatchDay"], sep = ", "
                         )
                     )
                     
                   })
          ),
          tags$script(
            "$(document).ready(function() {
              var div = document.getElementById('results-scroll');
              div.scrollLeft = div.scrollWidth - div.clientWidth;
            });"
          )
        )
        
      })
      #### Weekly TPE Leaders ####
      output$weeklyLeaders <- renderReactable({
        data <- topEarners()
        
        data %>% 
          then(
            onFulfilled = function(data){
              data %>% 
                reactable(
                  defaultColDef = colDef(minWidth = 75)
                )
            }
          )
      })
      
      #### Recently created ####
      output$created <- renderReactable({
        data <- getRecentCreates()
        
        data %>% 
          then(
            onFulfilled = function(data){
              data %>% 
                reactable()
            }
          )
      })
    }
  )
}