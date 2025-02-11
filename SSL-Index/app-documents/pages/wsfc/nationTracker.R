nationTrackerUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             h3("Nation Tracker of Players"),
             highchartOutput(ns("map")) %>% 
               withSpinnerMedium()
             )
    ),
    fluidRow(
      column(width = 12,
             h3("Regional Rosters"),
             uiOutput(ns("tabs")) %>% 
               withSpinnerMedium())
    )
  )
}

nationTrackerServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      #### DATA GENERATION ####
      
      orgReactable <- function(data){
        reactable(
          data,
          defaultColDef = colDef(header = function(value){str_to_upper(value)}),
          pagination = FALSE,
          columns = list(
            bankBalance = colDef(width = 120, format = colFormat(digits = 0, separators = TRUE, currency = "USD")),
            region = colDef(show = FALSE),
            name = colDef(width = 150, cell = function(value) tippy(value, tooltip = value, theme = "ssl", arrow = TRUE)),
            username = colDef(width = 120, cell = function(value) tippy(value, tooltip = value, theme = "ssl", arrow = TRUE)),
            discord = colDef(width = 120, cell = function(value) tippy(value, tooltip = value, theme = "ssl", arrow = TRUE)),
            render = colDef(width = 150, cell = function(value) tippy(value, tooltip = value, theme = "ssl", arrow = TRUE)),
            class = colDef(width = 75),
            tpe = colDef(width = 50),
            tpebank = colDef(width = 75),
            userStatus = colDef(width = 125),
            playerStatus = colDef(width = 140),
            team = colDef(name = "", width = 200, align = "left", cell = function(value){
              image <- img(src = sprintf("%s.png", value), style = "height: 30px;", alt = value, title = value)  
              
              list <- 
                tagList(
                  div(
                    class = "tableClubName",
                    div(style = "display: inline-block; width: 30px;", image),
                    span(value)  
                  )
                )
            })
          )
        )
      }
      
      data("worldgeojson")
      
      players <- reactive({
        readAPI(url = "https://api.simulationsoccer.com/player/getAllPlayers", query = list(active = "true"))  %>% 
          select(name, class, tpe, tpebank, username, discord, bankBalance, nationality, position, userStatus, playerStatus, render, team, region) %>% 
          future_promise()
      })
      
      nations <- reactive({
        data <- 
          readAPI(url = "https://api.simulationsoccer.com/player/getAllPlayers", query = list(active = "true")) %>%
          mutate(
            nationality = case_when(
              nationality == "United States" ~ "United States of America",
              nationality == "Korea, South" ~ "South Korea",
              nationality == "Czechia" ~ "Czech Republic",
              nationality %in% c("England", "Scotland", "Wales", "Northern Ireland") ~ "United Kingdom",
              TRUE ~ nationality
            )
          ) %>% 
          future_promise()
        
        data %>% 
          then(
            onFulfilled = function(data){
              sumActive <- 
                data %>% 
                group_by(nationality, userStatus, region) %>% 
                summarize(actives = n()) %>% 
                group_by(nationality) %>% 
                mutate(n = sum(actives)) %>% 
                group_by(region) %>% 
                mutate(roster = sum(actives))
              
              sumActive %>% 
                ungroup() %>% 
                pivot_wider(names_from = userStatus, values_from = actives) %>% 
                mutate(
                  across(c(Inactive, Active), ~replace_na(.x, 0))#,
                  # interval = case_when(
                  #   n < 6 ~ "1-5",
                  #   n < 11 ~ "6-10",
                  #   n < 16 ~ "11-15",
                  #   TRUE ~ "15+"
                  # ) %>% 
                  #   factor(levels = c("1-5", "6-10", "11-15", "15+"))
                )
            }
          )
      })
      
      
      output$map <- renderHighchart({
        nations() %>% 
          then(
            onFulfilled = function(table){
              highchart(
                hc_opts = list(
                    backgroundColor = "#000",
                    zooming = list(type = "xy", singleTouch = TRUE)
                  ),
                  type = "map"
                ) %>% 
                hc_title("Nation Tracker of Players") %>% 
                hc_add_series_map(
                  name = "Nr. Players",
                  worldgeojson, table, value = "n", joinBy = c("name", "nationality")
                ) %>% 
                hc_colorAxis(
                  dataClasses = 
                    color_classes(
                      c(1, 5, 10, 15, 20, 100),
                      colors = c("#D96F68", "#F5D17E", "#66B38C")
                      )
                  ) %>%
                hc_tooltip(HTML = TRUE, 
                           pointFormat = "{point.nationality} has {point.Active} active and {point.Inactive} inactive players.<br>In total the region {point.region} has {point.roster} players.") %>%
                hc_mapNavigation(enabled = TRUE)
            }
          )
        
      })
      
      output$tabs <- renderUI({
        players() %>% 
          then(
            onFulfilled = function(data){
              do.call(tabsetPanel, 
                      c(list(width = NULL), 
                        lapply(unique(data$region) %>% sort(), function(org) {
                          tabPanel(
                            title = org,
                            column(12,
                                   fluidRow(
                                     uiOutput(session$ns(paste0("overview_", org)))
                                   )
                            )
                          )
                        })
                      )
              )
            }
        )
      })
      
      observe({
        players() %>% 
          then(
            onFulfilled = function(data){
              lapply(unique(data$region) %>% sort(), function(i) {
                output[[paste0("overview_", i)]] <- renderUI({
                  roster <- data %>% filter(region == i)
                  
                  orgReactable(roster)
                })
              })
            }
          )
      })
      
    }
  )
}