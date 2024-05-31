submitPTUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width = 12,
      p("The .csv file should contain the username and the tpe gained."),
      fileInput(inputId = ns("gradedTask"),label = "Upload a , separated .csv file", accept = ".csv"),
      textInput(inputId = ns("taskName"), label = "What is the task name?")
    ) %>% 
      fluidRow(),
    column(
      width = 12,
      p("Rows marked in red have usernames that cannot be found on the forum. Check the spelling of the username and reupload the file."),
      reactableOutput(outputId = ns("checkImport"))
    ) %>% 
      fluidRow(),
    column(
      width = 12,
      actionButton(inputId = ns("submitTask"), label = "Submit")
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
          
          file <- read.csv(file$datapath, header = TRUE)
          
          if(all(c("username", "tpe") %in% colnames(file))){
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
                          getPlayerName(uid = x)
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
      
      observe({
        gradedTask() %>% 
          then(
            onFulfilled = function(data){
              data %>% 
                mutate(
                  source = input$taskName
                ) %>% 
                ptGradingVerify(session = session)
            }
          )
      }) %>% 
        bindEvent(
          input$submitTask
        )
      
      observe({
        
        data <- 
          gradedTask() %>% 
            mutate(
              uid = userinfo$uid
            )
          
        
        tpeLog(uid = userinfo$uid, pid = value$pid, tpe = tpeSummary)
        
        updateTPE(pid = value$pid, tpe = tpeSummary)
        
        showToast(type = "success", "You have successfully submitted a graded PT task.")
        
      }) %>% 
        bindEvent(
          input$confirmSubmission
        )
      
    }
  )
}