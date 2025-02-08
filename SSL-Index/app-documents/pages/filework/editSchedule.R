editScheduleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(2),
      column(4, h3("Team")),
      column(2, h3("Score"))
    ),
    fluidRow(
      column(2, h4("Game")),
      column(2, h4("Home")),
      column(2, h4("Away")),
      column(1, h4("Home")),
      column(1, h4("Away")),
      column(1, h4("ET")),
      column(1, h4("P")),
      column(2)
    ),
    uiOutput(ns("schedule")) %>% 
      withSpinnerMedium()
  )
}

editScheduleServer <- function(id, userinfo) {
  moduleServer(
    id,
    function(input, output, session) {
      updated <- reactiveVal({0})
      
      data <- reactive({
        getUnfinishedSchedule() %>% 
          then(
            onFulfilled = function(data){
              data[1:min(8, nrow(data)),]
            }
          )
      }) %>% 
        bindEvent(
          updated()
        )
      
      teams <- reactive({
        readAPI("https://api.simulationsoccer.com/organization/getOrganizations") %>% 
          filter(ID >= 0) %>% 
          add_row(tibble(name = c("Canada", "Benelux", "Eurasia", "Norden", "West Africa",
                                  "South America", "Oceania", "Pyrenees", "Alpen", 
                                  "East Europe", "British Isles", "Central America", 
                                  "East Africa", "Asia", "Central Europe", "USA"))) %>% 
          future_promise()
      })
      
      # Render tables dynamically for each organization
      output$schedule <- renderUI({
        
        promise_all(
          schedule = data(),
          teams = teams()
        ) %...>% 
          with({
            if(nrow(schedule) != 0){
              lapply(1:nrow(schedule), function(i) {
                game <- schedule[i, ]
                
                id <- game$gid
                
                fluidRow(
                  column(2, h5(paste(paste0("S", game$Season), if_else(game$Matchtype == 0, "CUP", paste0(game$Matchtype)), paste0("MD", game$Matchday)))),
                  column(2, selectInput(paste0("home_", id) %>% session$ns(), label = NULL, choices = c("", teams$name), selected = game$Home)) %>% div(class = "smallSelect"),
                  column(2, selectInput(paste0("away_", id) %>% session$ns(), label = NULL, choices = c("", teams$name), selected = game$Away)) %>% div(class = "smallSelect"),
                  column(1, selectInput(paste0("hscore_", id) %>% session$ns(), label = NULL, choices = c("None" = "NULL", 0:20), selected = game$HomeScore)) %>% div(class = "smallSelect"),
                  column(1, selectInput(paste0("ascore_", id) %>% session$ns(), label = NULL, choices = c("None" = "NULL", 0:20), selected = game$AwayScore)) %>% div(class = "smallSelect"),
                  column(1, checkboxInput(paste0("et_", id) %>% session$ns(), label = NULL, value = FALSE)),
                  column(1, checkboxInput(paste0("p_", id) %>% session$ns(), label = NULL, value = FALSE)),
                  column(2, actionButton(paste0("update_", id) %>% session$ns(), "Update"))
                )
              })
            } else {
              fluidRow(
                column(12, "No edits need to be made in the schedule.")
              )
            }
          })
      })
      
      observe({
        data() %>% 
          then(
            onFulfilled = function(data){
              lapply(data$gid, function(i) {
                
                observe({
                  home <- input[[paste0("home_", i)]]
                  away <- input[[paste0("away_", i)]]
                  hscore <- input[[paste0("hscore_", i)]]
                  ascore <- input[[paste0("ascore_", i)]]
                  et <- input[[paste0("et_", i)]]
                  p <- input[[paste0("p_", i)]]
                  
                  edits <- list(home = home, away = away, hscore = hscore, ascore = ascore, et = et, p = p)
                    
                  editUnfinishedSchedule(gid = i, edits = edits)
                  
                  updated(updated() + 1)
                  
                  showToast(type = "success", "The schedule has been updated!")
                }) %>% 
                  bindEvent(
                    input[[paste0("update_", i)]]
                  )
                
              })
            }
          )
      }) %>% 
        bindEvent(
          updated(),
          once = TRUE
        )
      
    }
  )
}