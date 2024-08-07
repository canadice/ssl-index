playerInfoBoxUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width = 12,
      fluidRow(
        column(
          width = 5,
          uiOutput(ns("name")),
          uiOutput(ns("bank"))
        ),
        column(width = 5, 
               uiOutput(ns("traits")),
               uiOutput(ns("positions"))),
        column(
          width = 2,
          imageOutput(ns("team"), height = NULL)
        )  
      ) %>% 
        withSpinnerSmall()
    )
  )
}

playerInfoBoxServer <- function(id, pid, mainSession) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$name <- renderUI({
        promise_all(
          name = getPlayerName(pid = pid),
          player = getPlayerStatus(pid = pid),
          user = getUserStatus(pid = pid)
        ) %...>% 
          with(
            tagList(
              h2(name$name %>% paste(paste("(", name$class, ")", sep = ""), sep = ", ")),
              h4(paste("Player: "), player$desc),
              h4(paste("User: "), user$desc)
            )
          )
        
      })
      
      output$bank <- renderUI({
        getBankTotal(pid = pid) %>% 
          then(
            onFulfilled = function(bank){
              tagList(
                h4(paste("Bank Balance: ") %>% HTML()),
                actionLink(inputId = session$ns("gotobank"), label = bank$balance %>% dollar())  
              )
            }
          )
      })
      
      observe({
        if(mainSession$input$tabs != "bankOverview"){
          updateTabItems(session = mainSession, "tabs", selected = "bankOverview")  
        }
      }) %>% 
        bindEvent(
          input$gotobank
        )
      
      
      output$traits <- renderUI({
        getPlayerTraits(pid = pid) %>% 
          then(
            onFulfilled = function(value){
              tagList(
                h4("Player Traits"),
                if(value %>% length() == 0){
                  "No traits"
                } else {
                  paste(value, collapse = "<br>") %>% HTML()
                }
              )
            },
            onRejected = function(error){
              print("something is wrong")
            }
          )
      })
      
      output$positions <- renderUI({
        getPlayerPositions(pid = pid) %>% 
          then(
            onFulfilled = function(value){
              tagList(
                h4("Primary Position(s)"),
                value %>% filter(value == 20) %>% select(name) %>% unlist() %>% paste(collapse = ", ") %>% HTML(),
                h4("Secondary Position(s)"),
                value %>% filter(value < 20, value >= 10) %>% select(name)  %>% unlist() %>% paste(collapse = ", ") %>% HTML(),
              )
            },
            onRejected = function(error){
              print("something is wrong")
            }
          )
      })
      
      output$team <- renderImage({
        getPlayerTeam(pid) %>%
          then(
            onFulfilled = function(value){
              list(
                src = normalizePath(file.path("./www", sprintf("%s.png", value$team))),
                width = 100,
                height = 100,
                alt = value$team
              )
            }
          )
      },
      deleteFile = FALSE)
    }
  )
}
