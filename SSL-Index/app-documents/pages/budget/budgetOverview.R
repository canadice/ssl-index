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
      
      transactions <- reactive({
        getOrgTransactions()
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
                                     uiOutput(session$ns(paste0("overview_", org)))
                                   ),
                                   fluidRow(
                                     em("Hover the salary for active clauses during each season."),
                                     h4("Major League"),
                                     reactableOutput(session$ns(paste0("table_", org, 1))),
                                     h4("Minor League"),
                                     reactableOutput(session$ns(paste0("table_", org, 2))),
                                     column(6,
                                            h4("Trades"),
                                            reactableOutput(session$ns(paste0("trades_", org)))
                                     ),
                                     column(6,
                                            h4("Signings"),
                                            reactableOutput(session$ns(paste0("signings_", org)))
                                     )
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
                        pagination = FALSE,
                        defaultColDef = colDef(html = TRUE, footerStyle = list(fontWeight = "bold")),
                        columns = list(
                          POSITION = colDef(width = 80),
                          PLAYER = colDef(footer = function(values) paste(sum(str_detect(values, "[A-z]+")), "Players")),
                          CLASS = colDef(name = "AGE", width = 50),
                          SIGNED = colDef(width = 75),
                          TPE = colDef(width = 50, footer = function(values) paste("AVG:", mean(values, na.rm = TRUE) %>% round(0))),
                          STATUS_C = colDef(name = "TYPE", width = 90, footer = function(values) paste(sum(str_detect(values, "IA")), "IA Contracts")),
                          SALARY0 = colDef(
                            width = 75,
                            name = paste0("S", currentSeason$season),
                            cell = function(value, index){
                              tippy(value %>% dollar(suffix = "M"), tooltip = data$CLAUSE0[index])
                            },
                            style = "border-left: 1px;",
                            footer = function(values) sprintf("$%.2fM", sum(values, na.rm = TRUE))
                          ),
                          SALARY1 = colDef(
                            width = 75,
                            name = paste0("S", currentSeason$season+1),
                            cell = function(value, index){
                              tippy(value %>% dollar(suffix = "M"), tooltip = data$CLAUSE1[index])
                            },
                            style = "border-left: 1px;",
                            footer = function(values) sprintf("$%.2fM", sum(values, na.rm = TRUE))
                          ),
                          SALARY2 = colDef(
                            width = 75,
                            name = paste0("S", currentSeason$season+2),
                            cell = function(value, index){
                              tippy(value %>% dollar(suffix = "M"), tooltip = data$CLAUSE2[index])
                            },
                            style = "border-left: 1px;",
                            footer = function(values) sprintf("$%.2fM", sum(values, na.rm = TRUE))
                          ),
                          SALARY3 = colDef(
                            width = 75,
                            name = paste0("S", currentSeason$season+3),
                            cell = function(value, index){
                              tippy(value %>% dollar(suffix = "M"), tooltip = data$CLAUSE3[index])
                            },
                            style = "border-left: 1px;",
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
      
      observe({
        budget() %>% 
          then(
            onFulfilled = function(budget){
              lapply(unique(budget$organization), function(i) {
                output[[paste0("overview_", i)]] <- renderUI({
                    data <- 
                      budget %>% 
                      filter(organization == i) %>% 
                      mutate(
                        across(
                          contains("salary"),
                          ~.x / 1000000
                        ),
                        status_c = if_else(status_c == 0, "IA", "ACT")
                      ) 
                    
                    majors <- data %>% filter(affiliate == 1)
                    minors <- data %>% filter(affiliate == 2)
                    
                    tagList(
                      column(3, 
                             h3("Salary Cap"),
                             h4("Majors: ", paste0("$", 55 - sum(majors$salary0, na.rm = TRUE), "M"), class = if_else((55 - sum(majors$salary0, na.rm = TRUE)) < 0, "Retired", "Active")),
                             h4("Minors: ", paste0("$", 45 - sum(minors$salary0, na.rm = TRUE), "M"), class = if_else((55 - sum(majors$salary0, na.rm = TRUE)) < 0, "Retired", "Active"))
                      ),
                      column(3,
                             h4("Cap Penalties and Bonuses"),
                             h5("Majors Bonus: "),
                             h5("Minors Bonus: "),
                             h5("Cap penalties: ")
                      ),
                      column(3,
                             )
                    )
                })
              })
            }
          )
      })
      
      observe({
        transactions() %>% 
          then(
            onFulfilled = function(transactions){
              lapply(0:7, function(i) {
                output[[paste0("trades_", i)]] <- renderReactable({
                  data <- 
                    transactions %>% 
                    filter(type == "TRAD") %>% 
                    select(!type) %>% 
                    group_by(tid) %>% 
                    filter(any(toOrg_player == i | toOrg_pick == i)) %>%
                    pivot_longer(
                      cols = name_player:orgName_pick,
                      names_to = c(".value", "pair"),
                      names_pattern = "(.*)_(.*?)",
                      values_transform = as.character
                    ) %>% 
                    unique() %>% 
                    group_by(tid, link, transfervalue, orgName) %>% 
                    summarize(
                      assets = paste0(name, collapse = ", ")
                    ) %>% 
                    mutate(
                      to = LETTERS[1:n()]
                    ) %>% 
                    group_by(tid, link, transfervalue) %>% 
                    pivot_wider(
                      names_from = to,
                      values_from = c(orgName, assets)
                    ) %>% 
                    mutate(
                      tid = sprintf('<a href="%s" target="_blank">%s</a>', link, tid)
                    ) %>% 
                    ungroup() %>% 
                    select(tid, transfervalue, contains("_A"), contains("_B"))
                    
                  data %>% 
                    reactable(
                      defaultColDef = colDef(html = TRUE),
                      columns = 
                        list(
                          orgName_A = colDef(
                            name = "Org. A"
                          ),
                          orgName_B = colDef(
                            name = "Org. B"
                          ),
                          assets_A = colDef(
                            name = "Receives"
                          ),
                          assets_B = colDef(
                            name = "Receives"
                          )
                        )
                    )
                })
              })
            }
          )
      })
    }
  )
}