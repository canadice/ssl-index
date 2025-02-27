bankDepositUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width = 12,
      column(width = 6,
             paste("The .csv file should contain the player name ('player') and the money gained ('amount'). The encoding of the file should be UTF-8. 
            If you are not sure how to do this, follow the instructions in this ", 
                   a("link", href = "https://stackoverflow.com/questions/18693139/how-to-convert-csv-files-encoding-to-utf-8"),
                   ".", sep = ""
             ) |> HTML()
             ),
      column(width = 3,
             downloadButton(ns("downloadTemplate"), label = "Download template file")
             ), 
      br(),
      br(),
      uiOutput(ns("informationUI"))
    ) |> 
      fluidRow(),
    column(
      width = 12,
      p("Rows marked in red have player names that cannot be found. Check the spelling of the player and reupload the file."),
      reactableOutput(outputId = ns("checkImport")) |> 
        withSpinnerMedium()
    ) |> 
      fluidRow(),
    column(
      width = 12,
      uiOutput(ns("confirmationUI"))
    ) |> 
      div(class = "frozen-bottom"),
    div(style = "min-height:100px;")
  )
}

bankDepositServer <- function(id, userinfo) {
  moduleServer(
    id,
    function(input, output, session) {
      
      bankDeposit <- reactive({
        if(input$bankDeposit |> is.null()){
          NULL
        } else {
          file <- input$bankDeposit
          
          file <- read.csv(file$datapath, header = TRUE, encoding = "UTF-8") 
          
          if(all(c("player", "amount") %in% (colnames(file) |> str_to_lower()))){
            colnames(file) <- colnames(file) |> str_to_lower()
            
            pids <- 
              promise_map(
                .x = file$player, 
                .f = getPlayerID
              ) |> 
              then(
                onFulfilled = function(pids){
                  pids <- 
                    lapply(pids, FUN = function(x){
                      if(x |> nrow() == 0){
                        -99
                      } else {
                        x
                      }
                    })
                  
                  tibble(
                    pid = pids |> do.call(what = rbind, args = .) |> unlist(),
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
      
      #### UI OUTPUTS ####
      output$confirmationUI <- renderUI({
        req(input$depositSource)
        req(input$bankDeposit)
        
        tagList(
          h3("Confirm the deposit:"),
          actionButton(inputId = session$ns("confirmDeposit"), label = "Confirm"),
          downloadButton(session$ns("downloadData"),label = "Fake", style = "visibility: hidden;")  
        )
      })
      
      output$informationUI <- renderUI({
        tagList(
          fileInput(inputId = session$ns("bankDeposit"),label = "Upload a , separated .csv file", accept = ".csv"),
          textInput(inputId = session$ns("depositSource"), label = "What is the source of the money?")
        )
      })
      
      output$checkImport <- renderReactable({
        if(bankDeposit() |> is.null()){
          NULL
        } else {
          req(input$depositSource)
          
          bankDeposit() |> 
            then(
              onFulfilled = function(data){
                data |> 
                  mutate(
                    source = input$depositSource
                  ) |> 
                  rename_with(
                    str_to_upper
                  ) |> 
                  reactable(
                    pagination = FALSE,
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
          bankDeposit() |> 
            then(
              onFulfilled = function(data){
                data |> 
                  filter(
                    pid == -99
                  ) |> 
                  select(!pid) |> 
                  write.csv(file, row.names = FALSE)
              }
            )
        })
      
      output$downloadTemplate <- downloadHandler(
        filename = function() { 
          paste("Bank Deposit Template.csv", sep="")
        },
        content = function(file) {
          url <- "https://raw.githubusercontent.com/canadice/ssl-index/main/SSL-Index/bankDepositTemplate.csv"  
          download.file(url, destfile = file)
        })
      
      observe({
        bankDeposit() |> 
          then(
            onFulfilled = function(data){
              processed <- 
                data |> 
                filter(
                  pid != -99
                )
              
              if(processed$pid |> duplicated() |> any()){
                showToast("error", "You have duplicated player ids in the submitted csv. Please check that you don't have the same player listed twice.")  
              } else if(any(data$pid == -99)){
                showToast("warning", "At least one player in the submitted csv cannot be found on the forum. They are found in the downloaded csv file for checking and re-import.")
                
                click("downloadData")
                
                data <- 
                  processed |> 
                    mutate(
                      source = input$depositSource
                    ) 
                
                addBankTransaction(uid = userinfo$uid, pid = data$pid, source = data$source, transaction = data$amount, status = 0)
                
                showToast(type = "success", "You have successfully deposited a subset of the transactions.")
                
                output$informationUI <- renderUI({
                  tagList(
                    fileInput(inputId = session$ns("bankDeposit"),label = "Upload a , separated .csv file", accept = ".csv"),
                    textInput(inputId = session$ns("depositSource"), label = "What is the source of the money?")
                  )
                })
              } else {
                data <- 
                  data |> 
                    mutate(
                      source = input$depositSource
                    ) 
                
                addBankTransaction(uid = userinfo$uid, pid = data$pid, source = data$source, transaction = data$amount, status = 0)
                
                showToast(type = "success", "You have successfully deposited the transaction.")
                
                output$informationUI <- renderUI({
                  tagList(
                    fileInput(inputId = session$ns("bankDeposit"),label = "Upload a , separated .csv file", accept = ".csv"),
                    textInput(inputId = session$ns("depositSource"), label = "What is the source of the money?")
                  )
                })
              }
            }
          )
      }) |> 
        bindEvent(
          input$confirmDeposit,
          ignoreInit = TRUE
        )
      
    }
  )
}
