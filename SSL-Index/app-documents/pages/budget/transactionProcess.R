tradeProcessUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, 
             box(
               title = "Organization A Receives",
               width = NULL,
               uiOutput(ns("organizationA"))
             )
           ),
      column(6, 
             box(
               title = "Organization B Receives",
               width = NULL,
               uiOutput(ns("organizationB"))
             )
      )
    ),
    fluidRow(
      div(
        class = "frozen-bottom",
        actionButton(
          inputId = ns("update"),
          label = "Add Transaction"
        )
      )
    )
  )
}

tradeProcessServer <- function(id, uid) {
  moduleServer(
    id,
    function(input, output, session) {
      pickAssets <- reactive({
        getPickAssets()
      })
      
      playerAssets <- reactive({
        getPlayerAssets()
      })
      
      organizations <- reactive({
        readAPI("https://api.simulationsoccer.com/organization/getOrganizations") %>% 
          select(id = ID, name = organization, abbr = abbreviation) %>% 
          filter(!is.na(name)) %>% 
          unique() %>% 
          future_promise()
      })
        
      
      #### OUTPUTS ####
      output$organizationA <- renderUI({
        organizations() %>% 
          then(
            onFulfilled = function(organizations){
              orgVector <- setNames(organizations$id, organizations$name)
              
              tagList(
                fluidRow(
                  column(6,
                         selectInput(session$ns("organizationA"), label = "Select organization", choices = c("", orgVector))
                  ),
                  column(6,
                         textInput(session$ns("link"), label = "Add link to transaction"),
                       )
                ),
                fluidRow(
                  uiOutput(session$ns("assetsA"))
                )
              )
            }
          )
      })
      
      output$organizationB <- renderUI({
        organizations() %>% 
          then(
            onFulfilled = function(organizations){
              orgVector <- setNames(organizations$id, organizations$name)
              
              tagList(
                fluidRow(
                  column(6,
                         selectInput(session$ns("organizationB"), label = "Select organization", choices = c("", orgVector))
                  )
                ),
                fluidRow(
                  uiOutput(session$ns("assetsB"))
                )
              )
            }
          )
      })
      
      
      lapply(LETTERS[1:2], function(group){
        observe({
          req(input[[paste0("organization", group)]])
          output[[paste0("assets", group)]] <- renderUI({
            promise_all(
              picks = pickAssets(),
              players = playerAssets()
            ) %...>% 
              with({
                pickVector<- 
                  setNames(
                    picks %>% filter(id != input[[paste0("organization", group)]]) %>% select(pickid) %>% unlist(), 
                    picks %>% filter(id != input[[paste0("organization", group)]]) %>% select(pick) %>% unlist()
                  )
                
                playerVector <- 
                  setNames(
                    players %>% filter(id != input[[paste0("organization", group)]]) %>% select(pid) %>% unlist(), 
                    players %>% filter(id != input[[paste0("organization", group)]]) %>% select(player) %>% unlist()
                  )
                
                
                tagList(
                  column(6,
                         h4("Players"),
                         lapply(0:5, function(i){
                           selectInput(session$ns(paste0("player", group, "_", i)), label = "Add asset", choices = c("", playerVector))  
                         })
                  ),
                  column(6,
                         h4("Picks"),
                         lapply(0:5, function(i){
                           selectInput(session$ns(paste0("pick", group, "_", i)), label = "Add asset", choices = c("", pickVector))  
                         })
                  )
                )
              })
          })
        }) %>% 
          bindEvent(input[[paste0("organization", group)]])
      }) 
      
      
      #### OBSERVE ####
      observe({
        if(
          ((input$playerB_0 %>% is.null()) & (input$pickB_0 %>% is.null())) |
          ((input$playerB_0 == "") & (input$pickB_0 == ""))
        ){
          showToast("error", "At least one asset must be sent by both teams.")
        } else {
          
          players <- tibble(
            player = c(input$playerA_0, input$playerA_1, input$playerA_2, input$playerA_3, input$playerA_4,
                       input$playerB_0, input$playerB_1, input$playerB_2, input$playerB_3, input$playerB_4),
            org = c(rep(input$organizationA, times = 5), rep(input$organizationB, times = 5))
          ) %>% 
            filter(
              player != ""
            )
          
          picks <- tibble(
            pick = c(input$pickA_0, input$pickA_1, input$pickA_2, input$pickA_3, input$pickA_4,
                     input$pickB_0, input$pickB_1, input$pickB_2, input$pickB_3, input$pickB_4),
            org = c(rep(input$organizationA, times = 5), rep(input$organizationB, times = 5))
          ) %>% 
            filter(
              pick != ""
            )
          
          transaction <- 
            tibble(
              link = paste0("'", input$link, "'"),
              type = paste0("'", input$transactionType, "'"),
              processed = uid
            )
          
          updateTransaction(transaction = transaction, players = players, picks = picks)
          
          showToast(type = "success", "The trade has been processed!")
        }
      }) %>% 
        bindEvent(
          input$update
        )
    }
  )
}