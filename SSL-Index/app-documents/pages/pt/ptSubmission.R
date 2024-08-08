submitPTUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width = 12,
      column(width = 6,
             paste("The .csv file should contain the username and the tpe gained. The encoding of the file should be UTF-8. 
            If you are not sure how to do this, follow the instructions in this ", 
                   a("link", href = "https://stackoverflow.com/questions/18693139/how-to-convert-csv-files-encoding-to-utf-8"),
                   ".", sep = ""
             ) %>% HTML()
      ),
      column(width = 3,
             downloadButton(ns("downloadTemplate"), label = "Download template file")
      ), 
      br(),
      br(),
      uiOutput(ns("informationUI"))
    ) %>% 
      fluidRow(),
    column(
      width = 12,
      p("Rows marked in red have usernames that cannot be found on the forum. Check the spelling of the username and reupload the file."),
      reactableOutput(outputId = ns("checkImport")) %>% 
        withSpinnerMedium()
    ) %>% 
      fluidRow(),
    div(style = "min-height:200px;"),
    column(
      width = 12,
      uiOutput(ns("confirmationUI")) %>% 
        div(class = "frozen-bottom")
    ) %>% 
      fluidRow()
  )
}

submitPTServer <- function(id, userinfo) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #### UI OUTPUTS ####
      output$confirmationUI <- renderUI({
        req(input$taskSource)
        req(input$taskName)
        
        tagList(
          h3("Confirm the submission:"),
          actionButton(inputId = session$ns("submitTask"), label = "Submit"),
          downloadButton(session$ns("downloadData"),label = "Fake", style = "visibility: hidden;")
        )
      })
      
      output$informationUI <- renderUI({
        tagList(
          fileInput(inputId = session$ns("taskSource"),label = "Upload a , separated .csv file", accept = ".csv"),
          textInput(inputId = session$ns("taskName"), label = "What is the task name?")
        )
      })
      
      
      #### REACTIVES ####
      taskSource <- reactive({
        if(input$taskSource %>% is.null()){
          NULL
        } else {
          file <- input$taskSource
          
          file <- read.csv(file$datapath, header = TRUE, encoding = "UTF-8") 
          
          if(all(c("username", "tpe") %in% (colnames(file) %>% str_to_lower()))){
            colnames(file) <- colnames(file) %>% str_to_lower()
            
            pids <- 
              promise_map(
              .x = file$username,
              .f = function(x){
                res <- getPlayerNameFromUsername(x)
                
                res %>% 
                  then(
                    onFulfilled = function(name){
                      if(name %>% nrow() == 0){
                        tibble(
                          pid = -99
                        )
                      } else {
                        name
                      }
                    }
                  )
              }
            ) %>% 
              then(
                onFulfilled = function(names){
                  names %>%
                    do.call(args = ., what = plyr::rbind.fill)
                }
              )
              
            pids %>%
              then(
                onFulfilled = function(data){
                  file %>%
                    mutate(
                      pid = data$pid %>% unlist(),
                      player = data$name %>% unlist()
                    )
                }
              )
          } else {
            showToast("error", "The file does not contain the headers 'username' and/or 'tpe'.")
            
            NULL
          }
            
        }
      })
      
      
      #### CHECKS OUTPUTS ####
      output$checkImport <- renderReactable({
        if(taskSource() %>% is.null()){
          NULL
        } else {
          req(input$taskName)
          
          if(nchar(input$taskName) > 32){
            shinyFeedback::feedbackDanger("taskName", show = TRUE, text = "You need to enter a shorter task name!")  
          } else {
            hideFeedback("taskName")
          }
          
          taskSource() %>% 
            then(
              onFulfilled = function(data){
                data %>% 
                  mutate(
                    source = input$taskName
                  ) %>% 
                  rename_with(str_to_upper) %>% 
                  reactable(
                    pagination = FALSE,
                    rowStyle = function(index){
                      if(.[index, "PID"] < 0){
                        list(background = "#FFCCCB", color = "black")
                      }
                    }
                  )
              }
            )
        }
      })
      
      #### DOWNLOAD BUTTON ####
      output$downloadData <- downloadHandler(
        filename = function() { 
          paste(input$taskName, " Unprocessed ", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          taskSource() %>% 
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
      
      output$downloadTemplate <- downloadHandler(
        filename = function() { 
          paste("PT Grading Template.csv", sep="")
        },
        content = function(file) {
          url <- "https://raw.githubusercontent.com/canadice/ssl-index/main/SSL-Index/ptGradeTemplate.csv"  # Replace with your actual URL
          download.file(url, destfile = file)
        })
      
      #### OBSERVERS ####
      observe({
        taskSource() %>% 
          then(
            onFulfilled = function(data){
              if(any(data$pid == -99)){
                showToast("warning", "At least one user in the submitted csv cannot be found on the forum. They are found in the downloaded csv file for checking and re-import.")
                
                click("downloadData")
                
                processed <- 
                  data %>% 
                  filter(
                    pid != -99
                  )
                
                processed %>% 
                  mutate(
                    source = input$taskName
                  ) %>% 
                  ptGradingVerify(session = session)
                
              } else if(data$pid %>% duplicated() %>% any()){
                showToast("error", "You have duplicated player ids in the submitted csv. Please check that you don't have the same user listed twice.")
              } else if(nchar(input$taskName) > 32){
                showToast("error", "You need to shorten the task name.")
                
                shinyFeedback::feedbackDanger("taskName", show = TRUE, text = "You need to enter a shorter task name!")  
              } else {
                data %>% 
                  mutate(
                    source = input$taskName
                  ) %>% 
                  ptGradingVerify(session = session)
              }
            }
          )  
      }) %>% 
        bindEvent(
          input$submitTask,
          ignoreInit = TRUE
        )
      
      observe({
        taskSource() %>% 
          then(
            onFulfilled = function(data){
              removeModal()
              
              data <- 
                data %>% 
                filter(
                  pid != -99
                )
              
              data %>% 
                mutate(
                  source = input$taskName
                ) %>% 
                apply(
                  X = .,
                  MARGIN = 1,
                  FUN = function(row){
                    tpeSummary <- tibble(tpe = row["tpe"], source = row["source"])
                    
                    tpeLog(uid = userinfo$uid, pid = row["pid"], tpe = tpeSummary)
                    updateTPE(pid = row["pid"], tpe = tpeSummary)
                  }
                )
              
              showToast(type = "success", "You have successfully submitted a graded PT task.")
              
              sendGradedTPE(source = input$taskName, tpe = data)
              # sendTest()
              
              output$informationUI <- renderUI({
                tagList(
                  fileInput(inputId = session$ns("taskSource"),label = "Upload a , separated .csv file", accept = ".csv"),
                  textInput(inputId = session$ns("taskName"), label = "What is the task name?")
                )
              })
            }
          )
      }) %>% 
        bindEvent(
          input$confirmSubmission,
          ignoreInit = TRUE
        )
      
    }
  )
}