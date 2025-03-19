academyIndexUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      ## First row
      fluidRow(
        column(
          width = 4,
          selectInput(
            inputId = ns("selectedSeason"),
            label = "Select a season",
            choices = 
              13:currentSeason$season %>% 
              sort(decreasing = TRUE)
          )
        )
      ),
      ## Second row
      fluidRow(
        h1("Outfield"),
        tabsetPanel(
          tabPanel("Statistics",
                   reactableOutput(ns("outfieldBasic")) %>% 
                     withSpinnerMedium()),
          tabPanel("Adv. Statistics",
                   reactableOutput(ns("outfieldAdvanced")) %>% 
                     withSpinnerMedium()))
        ),
      fluidRow(
        h1("Keeper"),
        tabsetPanel(
          tabPanel("Statistics",
                   reactableOutput(ns("keeperBasic")) %>% 
                     withSpinnerMedium()),
          tabPanel("Adv. Statistics",
                   reactableOutput(ns("keeperAdvanced")) %>% 
                     withSpinnerMedium()))
        )
    ) # close fluidpage
  ) # close tagList
}

academyIndexServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      #### DATA GENERATION ####
      outfieldData <- reactive({
        req(input$selectedSeason)
        readAPI("https://api.simulationsoccer.com/index/academyOutfield", query = list(season = input$selectedSeason)) %>% 
          future_promise()
      })
      
      keeperData <- reactive({
        req(input$selectedSeason)
        readAPI("https://api.simulationsoccer.com/index/academyKeeper", query = list(season = input$selectedSeason)) %>% 
          future_promise()
      })
      
      #### REACTABLE OUTPUT ####
      output$outfieldBasic <- renderReactable({
        outfieldData() %>% 
          then(
            onFulfilled = function(data){
              if(data %>% is_empty()){
                # SHOW NOTHING
              } else {
                currentData <- 
                  data %>% 
                  select(
                    name:assists, `shots on target`:offsides, blocks, `shots blocked`, `average rating`
                  ) 
                
                currentData %>% 
                  indexReactable()
              }
            }
          )
          
      })  
      
      output$outfieldAdvanced <- renderReactable({
        outfieldData() %>% 
          then(
            onFulfilled = function(data){
              if(data %>% is_empty()){
                # SHOW NOTHING
              } else {
                currentData <- 
                  data %>% 
                  select(
                    name:club, 
                    xg,
                    xa:`fk shots`,
                    `open play key passes`:`goals outside box`,
                    `press%`:`pen adj xG`
                  ) 
                
                currentData %>% 
                  indexReactable()
              }
            }
          )
        
      }) 
      
      output$keeperBasic <- renderReactable({
        keeperData() %>% 
          then(
            onFulfilled = function(data){
              if(data %>% is_empty()){
                # SHOW NOTHING
              } else {
                currentData <- 
                  data %>% 
                  select(
                    name:`save%`
                  ) 
                
                currentData %>% 
                  indexReactable()
              }
            }
          )
        
      })  
      
      output$keeperAdvanced <- renderReactable({
        keeperData() %>% 
          then(
            onFulfilled = function(data){
              if(data %>% is_empty()){
                # SHOW NOTHING
              } else {
                currentData <- 
                  data %>% 
                  select(
                    name:club, 
                    `penalties faced`:`xg prevented`
                  ) 
                
                currentData %>% 
                  indexReactable()
              }
            }
          )
        
      }) 
      
    }
  )
}