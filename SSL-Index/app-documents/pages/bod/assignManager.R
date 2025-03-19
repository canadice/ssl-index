assignManagerUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(2, h3("Organization")),
      column(3, h3("Organizational Manager")),
      column(3, h3("Assistant Manager")),
      column(3, h3("Assistant Manager")),
      column(1)
    ),
    uiOutput(ns("orgTables")) %>% 
      withSpinnerMedium()
  )
}

assignManagerServer <- function(id, userinfo) {
  moduleServer(
    id,
    function(input, output, session) {
      updated <- reactiveVal({0})
      
      data <- reactive({
        readAPI("https://api.simulationsoccer.com/organization/getManagers") %>% 
          future_promise()
      }) %>% 
        bindEvent(
          updated()
        )
      
      allUsers <- 
        reactive({
          getUsers()
        })
      
      # Render tables dynamically for each organization
      output$orgTables <- renderUI({
        
        promise_all(
          data = data(),
          users = allUsers()
        ) %...>% 
          with({
            lapply(1:nrow(data), function(i) {
              org <- data[i, ]
              userVector <- setNames(users$uid, users$username)
              
              fluidRow(
                column(2, h5(org$name)),
                column(3, selectInput(paste0("manager_", org$id) %>% session$ns(), label = NULL, choices = c("", userVector), selected = org$orgManager)),
                column(3, selectInput(paste0("assistant1_", org$id) %>% session$ns(), label = NULL, choices = c("", userVector), selected = org$assManager1)),
                column(3, selectInput(paste0("assistant2_", org$id) %>% session$ns(), label = NULL, choices = c("", userVector), selected = org$assManager2)),
                column(1, actionButton(paste0("update_", org$id) %>% session$ns(), "Update"))
              )
            })
          })
      })
      
      lapply(0:7, function(i) {
        
        observe({
          orgManager <- input[[paste0("manager_", i)]]
          assManager1 <- input[[paste0("assistant1_", i)]]
          assManager2 <- input[[paste0("assistant2_", i)]]
          
          managers = c(orgManager, assManager1, assManager2)
          
          editManagers(managers = managers, id = i)
          
          updated(updated() + 1)
          
          showToast(.options = myToastOptions,type = "success", "The managers have been updated!")
        }) %>% 
          bindEvent(
            input[[paste0("update_", i)]]
          )
        
        })
    }
  )
}
