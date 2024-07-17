contractProcessUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             uiOutput(ns("selectPlayer"))       
             )
    ),
    fluidRow(
      column(4, 
             box(
               title = "Organization Info",
               width = NULL,
               uiOutput(ns("organization"))
             )
           ),
      column(8, 
             box(
               title = "Contract Info",
               width = NULL,
               uiOutput(ns("contract"))
             )
      )
    ),
    fluidRow(
      column(12,
             box(
               title = "Salary and Clauses Info",
               width = NULL,
               uiOutput(ns("salary"))
             )
           )
    ),
    fluidRow(
      div(
        class = "frozen-bottom",
        actionButton(
          inputId = ns("update"),
          label = "Update Contract"
        )
      )
    )
  )
}

contractProcessServer <- function(id, uid) {
  moduleServer(
    id,
    function(input, output, session) {
      updated <- reactiveVal({0})
      
      allNames <- reactive({
        getPlayerNames()
      })
      
      data <- reactive({
        req(input$selectedPlayer)
        getBudgetPlayer(pid = input$selectedPlayer)
      }) %>% 
        bindEvent(
          input$selectedPlayer,
          updated()
        )
      
      organizations <- reactive({
        getOrganizations()
      })
        
      
      #### OUTPUTS ####
      output$selectPlayer <- renderUI({
        allNames() %>% 
          then(
            onFulfilled = function(names) {
              names <- 
                names %>%
                filter(status_p > 0)
              
              namedVector <- names$pid
              
              names(namedVector) <- names$name
              
              selectInput(session$ns("selectedPlayer"), "Select Player", choices = namedVector)
            }
          )
      })
      
      output$organization <- renderUI({
        promise_all(
          budget = data(),
          organizations = organizations()
        ) %...>% 
          with({
            orgVector <- setNames(organizations$id, organizations$name)
            
            tagList(
              selectInput(session$ns("organization"), label = "Select organization", choices = c("", orgVector), selected = budget$org),
              selectInput(session$ns("affiliate"), label = "Select affiliate", choices = c("", "Major" = 1, "Minor" = 2), selected = budget$affiliate)
            )
          })
      })
      
      output$contract <- renderUI({
        data() %>% 
          then(
            onFulfilled = function(data){
            tagList(
              column(6, 
                     textInput(session$ns("link"), label = "Link to contract post", value = data$link),
                     numericInput(session$ns("signed"), label = "The season the contract was signed", value = data$signed, min = currentSeason$season - 4, max = currentSeason$season)
                   ),
              column(6,
                     selectInput(session$ns("type"), label = "Contract type", choices = c("", "Rookie" = "ROO", "IFA" = "IFA", "Free Agent" = "FA", "Extension" = "EXT"), selected = data$type),
                     numericInput(session$ns("length"), label = "Contract length", value = data$length, min = 1, max = 3),
                     checkboxInput(session$ns("active"), label = "Active contract?", value = data$status_c == 1)
                   )
            )
          })
      })
      
      output$salary <- renderUI({
        data() %>% 
          then(
            onFulfilled = function(data){
              tagList(
                column(3, 
                       autonumericInput(session$ns("salary0"), label = paste0('Salary S', currentSeason$season), value = data$salary0, minimumValue = 1000000, maximumValue = 7000000, step = 100000, currencySymbol = "$", currencySymbolPlacement = "p", digitGroupSeparator = ",", decimalPlaces = 0, wheelStep = 100000, wheelOn = "hover"),
                       selectizeInput(session$ns("clause0"), label = paste0('Clauses in S', currentSeason$season), choices = c("", "VET", "MAJ", "NMC"), multiple = TRUE, selected = data$clause0 %>% str_split(pattern = ",") %>% unlist())
                ),
                column(3, 
                       autonumericInput(session$ns("salary1"), label = paste0('Salary S', currentSeason$season+1), value = data$salary1, minimumValue = 1000000, maximumValue = 7000000, step = 100000, currencySymbol = "$", currencySymbolPlacement = "p", digitGroupSeparator = ",", decimalPlaces = 0, wheelStep = 100000, wheelOn = "hover"),
                       selectizeInput(session$ns("clause1"), label = paste0('Clauses in S', currentSeason$season+1), choices = c("", "VET", "MAJ", "NMC"), multiple = TRUE, selected = data$clause1 %>% str_split(pattern = ",") %>% unlist())
                ),
                column(3, 
                       autonumericInput(session$ns("salary2"), label = paste0('Salary S', currentSeason$season+2), value = data$salary2, minimumValue = 1000000, maximumValue = 7000000, step = 100000, currencySymbol = "$", currencySymbolPlacement = "p", digitGroupSeparator = ",", decimalPlaces = 0, wheelStep = 100000, wheelOn = "hover"),
                       selectizeInput(session$ns("clause2"), label = paste0('Clauses in S', currentSeason$season+2), choices = c("", "VET", "MAJ", "NMC"), multiple = TRUE, selected = data$clause2 %>% str_split(pattern = ",") %>% unlist())
                ),
                column(3, 
                       autonumericInput(session$ns("salary3"), label = paste0('Salary S', currentSeason$season+3), value = data$salary3, , minimumValue = 1000000, maximumValue = 7000000, step = 100000, currencySymbol = "$", currencySymbolPlacement = "p", digitGroupSeparator = ",", decimalPlaces = 0, wheelStep = 100000, wheelOn = "hover"),
                       selectizeInput(session$ns("clause3"), label = paste0('Clauses in S', currentSeason$season+3), choices = c("", "VET", "MAJ", "NMC"), multiple = TRUE, selected = data$clause3 %>% str_split(pattern = ",") %>% unlist())
                )
              )
            })
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