bankDepositUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width = 12,
      paste("The .csv file should contain the player name ('player') and the money gained ('amount'). The encoding of the file should be UTF-8. 
            If you are not sure how to do this, follow the instructions in this ", 
            a("link", href = "https://stackoverflow.com/questions/18693139/how-to-convert-csv-files-encoding-to-utf-8"),
            ".", sep = ""
      ) %>% HTML(), 
      br(),
      fileInput(inputId = ns("bankDeposit"),label = "Upload a , separated .csv file", accept = ".csv"),
      textInput(inputId = ns("depositSource"), label = "What is the source of the money?")
    ) %>% 
      fluidRow(),
    column(
      width = 12,
      p("Rows marked in red have player names that cannot be found. Check the spelling of the player and reupload the file."),
      reactableOutput(outputId = ns("checkImport")) %>% 
        withSpinnerMedium()
    ) %>% 
      fluidRow(),
    column(
      width = 12,
      h3("Confirm the deposit:"),
      actionButton(inputId = ns("confirmDeposit"), label = "Confirm"),
      downloadButton(ns("downloadData"),label = "Fake", style = "visibility: hidden;")
    ) %>% 
      div(class = "frozen-bottom")
  )
}

bankDepositServer <- function(id, userinfo) {
  moduleServer(
    id,
    function(input, output, session) {
      
      bankDeposit <- reactive({
        if(input$bankDeposit %>% is.null()){
          NULL
        } else {
          file <- input$bankDeposit
          
          file <- read.csv(file$datapath, header = TRUE, encoding = "UTF-8") 
          
          if(all(c("player", "amount") %in% (colnames(file) %>% str_to_lower()))){
            colnames(file) <- colnames(file) %>% str_to_lower()
            
            pids <- 
              promise_map(
                .x = file$player, 
                .f = getPlayerID
              ) %>% 
              then(
                onFulfilled = function(pids){
                  missing <- lapply(X = pids,FUN = function(x){
                    x %>% nrow() == 0
                  }) %>% 
                    unlist() %>% 
                    which()
                  
                  if(length(missing) != 0){
                    pids[[missing]] <- tibble(pid = -99)
                  }
                    
                  tibble(
                    pid = pids %>% do.call(what = rbind, args = .) %>% unlist(),
                    player = file$player,
                    amount = file$amount
                  )
                }
              )
          } else {
            showToast("error", "The file does not contain the headers 'player' and/or 'amount'.")
            
            NULL
          }
            
        }
      })
      
      output$checkImport <- renderReactable({
        if(bankDeposit() %>% is.null()){
          NULL
        } else {
          bankDeposit() %>% 
            then(
              onFulfilled = function(data){
                data %>% 
                  mutate(
                    source = input$depositSource
                  ) %>% 
                  rename_with(
                    str_to_upper
                  ) %>% 
                  reactable(
                    columns = 
                      list(
                        AMOUNT = colDef(format = colFormat(digits = 0, separators = TRUE, currency = "USD"))
                      ),
                    rowStyle = function(index){
                      if(.[index, "PID"] < 0){
                        list(background = "#FFCCCB")
                      }
                    }
                  )
              }
            )
        }
      })
      
      # DOWNLOAD BUTTON 
      output$downloadData <- downloadHandler(
        filename = function() { 
          paste(input$depositSource, " Unprocessed ", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          bankDeposit() %>% 
            then(
              onFulfilled = function(data){
                data %>% 
                  filter(
                    pid == -99
                  ) %>% 
                  select(!pid) %>% 
                  write.csv(file, row.names = FALSE)
              }
            )
        })
      
      observe({
        bankDeposit() %>% 
          then(
            onFulfilled = function(data){
              if(data$pid %>% duplicated() %>% any()){
                showToast("error", "You have duplicated player ids in the submitted csv. Please check that you don't have the same player listed twice.")  
              } else if(any(data$pid == -99)){
                showToast("warning", "At least one player in the submitted csv cannot be found on the forum. They are found in the downloaded csv file for checking and re-import.")
                
                click("downloadData")
                
                processed <- 
                  data %>% 
                  filter(
                    pid != -99
                  )
                
                data <- 
                  processed %>% 
                    mutate(
                      source = input$depositSource
                    ) 
                
                addBankTransaction(uid = userinfo$uid, pid = data$pid, source = data$source, transaction = data$amount, status = 0)
                
                showToast(type = "success", "You have successfully deposited a subset of the transactions.")
              } else {
                data <- 
                  data %>% 
                    mutate(
                      source = input$depositSource
                    ) 
                
                addBankTransaction(uid = userinfo$uid, pid = data$pid, source = data$source, transaction = data$amount, status = 0)
                
                showToast(type = "success", "You have successfully deposited the transaction.")
              }
            }
          )
      }) %>% 
        bindEvent(
          input$confirmDeposit,
          ignoreInit = TRUE
        )
      
    }
  )
}