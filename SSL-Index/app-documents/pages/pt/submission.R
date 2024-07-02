submitPTUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width = 12,
      paste("The .csv file should contain the username and the tpe gained. The encoding of the file should be UTF-8. 
            If you are not sure how to do this, follow the instructions in this ", 
            a("link", href = "https://stackoverflow.com/questions/18693139/how-to-convert-csv-files-encoding-to-utf-8"),
            ".", sep = ""
      ) %>% HTML(), 
      br(),
      fileInput(inputId = ns("gradedTask"),label = "Upload a , separated .csv file", accept = ".csv"),
      textInput(inputId = ns("taskName"), label = "What is the task name?")
    ) %>% 
      fluidRow(),
    column(
      width = 12,
      p("Rows marked in red have usernames that cannot be found on the forum. Check the spelling of the username and reupload the file."),
      reactableOutput(outputId = ns("checkImport")) %>% 
        withSpinnerMedium()
    ) %>% 
      fluidRow(),
    column(
      width = 12,
      actionButton(inputId = ns("submitTask"), label = "Submit"),
      downloadButton(ns("downloadData"),label = "Fake", style = "visibility: hidden;")
    )
  )
}

submitPTServer <- function(id, userinfo) {
  moduleServer(
    id,
    function(input, output, session) {
      
      gradedTask <- reactive({
        if(input$gradedTask %>% is.null()){
          NULL
        } else {
          file <- input$gradedTask
          
          file <- read.csv(file$datapath, header = TRUE, encoding = "UTF-8") 
          
          if(all(c("username", "tpe") %in% (colnames(file) %>% str_to_lower()))){
            colnames(file) <- colnames(file) %>% str_to_lower()
            
            pids <- 
              promise_map(
                .x = file$username, 
                .f = getUserID
              ) %>% 
              then(
                onFulfilled = function(uids){
                  uids %>%
                    promise_map(
                      .x = .,
                      .f = function(x) {
                        if(nrow(x) == 0){
                          tibble(
                            pid = -99
                          )
                        } else {
                          res <- getPlayerName(uid = x)
                          
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
                      } 
                    ) %>% 
                    then(
                      onFulfilled = function(names){
                        names %>%
                          do.call(args = ., what = plyr::rbind.fill) %>%
                          select(pid)
                      }
                    )
                }
              )
            
            pids %>%
              then(
                onFulfilled = function(data){
                  file %>%
                    mutate(
                      pid = data %>% unlist()
                    )
                }
              )
          } else {
            showToast("error", "The file does not contain the headers 'username' and/or 'tpe'.")
            
            NULL
          }
            
        }
      })
      
      output$checkImport <- renderReactable({
        if(gradedTask() %>% is.null()){
          NULL
        } else {
          gradedTask() %>% 
            then(
              onFulfilled = function(data){
                data %>% 
                  mutate(
                    source = input$taskName
                  ) %>% 
                  reactable(
                    rowStyle = function(index){
                      if(.[index, "pid"] < 0){
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
          paste(input$taskName, " Unprocessed ", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          gradedTask() %>% 
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
        gradedTask() %>% 
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
                showToast("error", "You hav duplicated player ids in the submitted csv. Please check that you don't have the same user listed twice.")
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
        
        gradedTask() %>% 
          then(
            onFulfilled = function(data){
              removeModal()
              
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