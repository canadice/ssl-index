budgetOverviewUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(12,
           uiOutput(ns("tabs"))
         )
    
  )
}

budgetOverviewServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      budget <- reactive({
        getBudget()
      })
      
      
      #### OUTPUT ####
      output$tabs <- renderUI({
        budget() %>% 
          then(
            onFulfilled = function(budget){
              do.call(tabsetPanel, 
                      c(list(width = NULL), 
                        lapply(unique(budget$organization), function(org) {
                          tabPanel(
                            title = org,
                            column(12,
                                   fluidRow(
                                     em("Hover the salary for active clauses during each season."),
                                     h4("Major League"),
                                     reactableOutput(session$ns(paste0("table_", org, 1))),
                                     h4("Minor League"),
                                     reactableOutput(session$ns(paste0("table_", org, 2)))
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
        budget() %>% 
          then(
            onFulfilled = function(budget){
              lapply(unique(budget$organization), function(i) {
                lapply(1:2, function(j){
                  output[[paste0("table_", i, j)]] <- renderReactable({
                    data <- 
                      budget %>% 
                      filter(organization == i, affiliate == j) %>% 
                      select(position, player, username, class, tpe, signed, link, status_c, contains("salary"), contains("clause")) %>% 
                      rename_with(str_to_upper) %>%
                      mutate(
                        across(
                          contains("salary"),
                          ~.x / 1000000
                        ),
                        STATUS_C = if_else(STATUS_C == 0, "IA", "ACT")
                      ) %>% 
                      mutate(SIGNED = sprintf('<a href="%s" target="_blank">S%s</a>', LINK, SIGNED)) %>%
                      select(-LINK)
                    
                    data %>% 
                      reactable(
                        defaultColDef = colDef(html = TRUE, footerStyle = list(fontWeight = "bold")),
                        columns = list(
                          POSITION = colDef(footer = function(values) paste(sum(str_detect(values, "[A-z]+")), "Players")),
                          CLASS = colDef(name = "AGE", width = 50),
                          SIGNED = colDef(width = 75),
                          TPE = colDef(width = 50, footer = function(values) paste("AVG:", mean(values, na.rm = TRUE) %>% round(0))),
                          STATUS_C = colDef(name = "TYPE", width = 80, footer = function(values) paste(sum(str_detect(values, "IA")), "IA Contracts")),
                          SALARY0 = colDef(
                            name = paste0("S", currentSeason$season),
                            cell = function(value, index){
                              tippy(value %>% dollar(suffix = "M"), tooltip = data$CLAUSE0[index])
                            },
                            footer = function(values) sprintf("$%.2fM", sum(values, na.rm = TRUE))
                          ),
                          SALARY1 = colDef(
                            name = paste0("S", currentSeason$season+1),
                            cell = function(value, index){
                              tippy(value %>% dollar(suffix = "M"), tooltip = data$CLAUSE1[index])
                            },
                            footer = function(values) sprintf("$%.2fM", sum(values, na.rm = TRUE))
                          ),
                          SALARY2 = colDef(
                            name = paste0("S", currentSeason$season+2),
                            cell = function(value, index){
                              tippy(value %>% dollar(suffix = "M"), tooltip = data$CLAUSE2[index])
                            },
                            footer = function(values) sprintf("$%.2fM", sum(values, na.rm = TRUE))
                          ),
                          SALARY3 = colDef(
                            name = paste0("S", currentSeason$season+3),
                            cell = function(value, index){
                              tippy(value %>% dollar(suffix = "M"), tooltip = data$CLAUSE3[index])
                            },
                            footer = function(values) sprintf("$%.2fM", sum(values, na.rm = TRUE))
                          ),
                          CLAUSE0 = colDef(show = FALSE),
                          CLAUSE1 = colDef(show = FALSE),
                          CLAUSE2 = colDef(show = FALSE),
                          CLAUSE3 = colDef(show = FALSE)
                        )
                      )
                  })
                })
              })
            }
          )
      })
    }
  )
}