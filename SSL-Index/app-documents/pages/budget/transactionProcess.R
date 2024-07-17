transactionProcessUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3,offset = 3,
             radioButtons(ns("transactionType"), label = "Select transaction type", choices = c("Trade", "FA/IFA"), inline = TRUE))
    ),
    fluidRow(
      column(6, 
             box(
               title = "Organization A Receives",
               width = NULL,
               uiOutput(ns("organizationA"))
             )
           ),
      column(6, 
             uiOutput(ns("organizationB"))
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

transactionProcessServer <- function(id, uid) {
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
        getOrganizations()
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
                         if(input$transactionType == "FA/IFA"){
                           autonumericInput(
                             session$ns("transfervalue"), 
                             label = tippy("Add transfer value", tooltip = "Only used if FA/IFA signing"), 
                             value = 0,
                             currencySymbol = "$", currencySymbolPlacement = "p", 
                             digitGroupSeparator = ",", decimalPlaces = 0
                           )
                         }
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
        if(input$transactionType == "Trade"){
          organizations() %>% 
            then(
              onFulfilled = function(organizations){
                orgVector <- setNames(organizations$id, organizations$name)
                
                tagList(
                  box(
                    title = "Organization B Receives",
                    width = NULL,
                    fluidRow(
                      column(6,
                             selectInput(session$ns("organizationB"), label = "Select organization", choices = c("", orgVector))
                      )
                    ),
                    fluidRow(
                      uiOutput(session$ns("assetsB"))
                    )
                  )
                )
              }
            )
        }
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
        summary <- 
          tibble(
            pid = input$selectedPlayer,
            org = input$organization,
            affiliate = input$affiliate,
            link = paste0("'", input$link, "'"),
            signed = input$signed,
            type = paste0("'", input$type, "'"),
            length = input$length,
            salary0 = input$salary0,
            salary1 = input$salary1,
            salary2 = input$salary2,
            salary3 = input$salary3,
            clause0 = paste0("'", paste0(input$clause0, collapse = ","), "'"),
            clause1 = paste0("'", paste0(input$clause1, collapse = ","), "'"),
            clause2 = paste0("'", paste0(input$clause2, collapse = ","), "'"),
            clause3 = paste0("'", paste0(input$clause3, collapse = ","), "'"),
            processed = uid,
            status_c = input$active
          )
        
        updateContract(summary)
        
        updated(updated() + 1)
        
        showToast(type = "success", "The contract has been updated!")
      }) %>% 
        bindEvent(
          input$update
        )
    }
  )
}