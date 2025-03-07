organizationPagesUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    column(12,
           h2("Organization Overview"),
           uiOutput(ns("tabs")) |> 
             withSpinnerMedium()
    ) 
  ) # close tagList
  
}

organizationPagesServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      
      
      players <- reactive({
        readAPI(url = "https://api.simulationsoccer.com/player/getAllPlayers") |> 
          select(name, class, tpe, tpebank, username, discord, bankBalance, nationality, position, userStatus, playerStatus, render, team, affiliate) |> 
          future_promise()
      })
      
      organizations <- readAPI("https://api.simulationsoccer.com/organization/getOrganizations") |> 
        filter(!is.na(organization))
      
      output$tabs <- renderUI({
        do.call(tabsetPanel, 
                c(list(width = NULL, selected = "CA Buenos Aires"), 
                  lapply(unique(organizations$organization), function(org) {
                    tabPanel(
                      title = org,
                      column(12,
                             fluidRow(
                               uiOutput(session$ns(paste0("overview_", unique(organizations$ID[organizations$organization == org]))))
                             )
                      )
                    )
                  })
                )
        )
      })
        
      observe({
        players() |> 
          then(
            onFulfilled = function(data){
              lapply(unique(organizations$ID), function(i) {
                output[[paste0("overview_", i)]] <- renderUI({
                  majors <- data |> filter(team %in% organizations$name[organizations$ID == i] & affiliate == 1)
                  minors <- data |> filter(team %in% organizations$name[organizations$ID == i] & affiliate == 2)
                  
                  tagList(
                    
                    h3("Majors"),
                    fluidRow(
                      orgReactable(majors)
                    ),
                    
                    if(nrow(minors) > 0){
                      tagList(
                        h3("Minors"),
                        fluidRow(
                          orgReactable(minors)
                        )
                      )
                    }
                  )
                })
              })
            }
          )
      })
      
      
    }
  )
}
