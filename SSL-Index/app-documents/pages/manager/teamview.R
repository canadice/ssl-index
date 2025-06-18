managerTeamUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             box(title = "Your Team Overview", width = NULL, solidHeader = TRUE,
                 reactableOutput(ns("teamOverview")) %>% 
                   withSpinnerMedium(),
                 br(),
                 actionButton(ns("goRegress"), "Regress Player")
             )
             
      )
    ),
    fluidRow(
      column(
        width = 12,
        box(title = "Regress player", collapsible = TRUE, width = NULL,
            playerInfoBoxUI(ns("playerInfo")),
            playerAttributeBoxUI(ns("playerAttributes"))
        )
      )
    ) %>% 
      div(id = ns("selectedPlayer")) %>% 
      hidden()
  )
}

managerTeamServer <- function(id, userinfo) {
  moduleServer(
    id,
    function(input, output, session) {
      
      rv <- reactiveValues(
        bank = 0
      )
      
      playerData <- reactive({
        getPlayersFromTeam(userinfo$uid) 
      }) %>% 
        bindEvent(
          rv$bank,
          ignoreInit = FALSE
        )
      
      output$teamOverview <- renderReactable({
         playerData() %>% 
          then(
            onFulfilled = function(value){
              value %>% 
                select(!pid) %>%
                reactable(
                  selection = "single",
                  onClick = "select",
                  groupBy = "affiliate",
                  defaultColDef = colDef(header = function(value){str_to_upper(value)}),
                  rowStyle = function(index){
                    if(.[index, "tpebank"] < 0){
                      list(background = "#FFCCCB", color = "black")
                    }
                  }
                )
            }
          ) 
      })
      
      observe({
        shinyjs::hide("selectedPlayer")
      }) %>% 
        bindEvent(
          rv$bank,
          ignoreInit = FALSE
        )
      
      observe({
        selected <- getReactableState("teamOverview", "selected")
        req(selected)
        
        playerData() %>% 
          then(
            onFulfilled = function(value){
              selection <- value[selected,]
              
              if(selection$tpebank < 0){
                shinyjs::show("selectedPlayer")
                
                playerInfoBoxServer(id = paste0("playerInfo", selection$pid), pid = selection$pid)
                
                playerAttributeBoxServer(
                  id = paste0("playerAttributes", selection$pid), 
                  parent = session, 
                  pid = selection$pid, 
                  uid = userinfo$uid,
                  rv = rv
                ) 
                
              } else {
                shinyjs::hide("selectedPlayer")
                showToast(.options = myToastOptions,type = "error", "The chosen player does not need to regress.")
              }
                    
            }
          )
        
      }) %>% 
        bindEvent(
          input$goRegress
        )
    }
  )
}
