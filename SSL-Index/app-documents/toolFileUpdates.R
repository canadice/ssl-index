
############################################################################
############################################################################
###                                                                      ###
###                      COMPARE PLAYERS IN THE SSL                      ###
###                                                                      ###
############################################################################
############################################################################


### UI module for player similarities using MDS
fileUpdateToolUI <- function(id){
  
  ## Creating the namespacing function for all IDs
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(
        p(
          "The following tool creates a downloadable JSON file for the selected player based on their 
          current build from the SSL Forum. Using FMRTE22, you can then import the updated build for the chosen player."
        ),
        
        selectInput(
          inputId = ns("player"),
          label = "Select a player",
          choices = 
            playerData %>% 
            filter(!(Team %in% c("Retired"))) %>% 
            select(Name) %>% 
            arrange(Name)
        ) %>% 
          column(width = 4),
        column(
          width = 8, 
          verbatimTextOutput(
            outputId = ns("playerExport")
          ),
          div(
            id = ns("playerDownload"),
            downloadButton(ns("downloadData"), "Download")
          ),
          reactableOutput(
            ns("playerBio")
          )
        )
      )
    )
  )
}

## Backend module for player similarities
fileUpdateToolSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){
      con <- 
        dbConnect(
          SQLite(), 
          dbFile
        )
      
      selectedData <- reactive({
        filterName <- input$player
        
        temp <- 
          tbl(con, "Daily_Scrape") %>% 
          filter(
            Name == filterName
          ) %>% 
          collect()
        
        paste(
          '{"GoalKeeperAttributes":{',
          sapply(
            temp %>% 
              select(`Aerial Reach`:Throwing),
            FUN = function(x) {if_else(is.na(x), 5, x)}
          ) %>% 
            paste(paste('"', names(.) %>% str_to_title(), '"', sep = ""), ., sep = ":", collapse = ",") %>% 
            str_replace_all(pattern = " ", replacement = "") %>% 
            str_replace(pattern = "TendencyToRush", replacement = "RushingOut") %>% 
            str_replace(pattern = "AerialReach", replacement = "AerialAbility"),
          '},"MentalAttributes":{',
          sapply(
            temp %>% 
              select(`Aggression`:`Work Rate`),
            FUN = function(x) {if_else(is.na(x), 5, x)}
          ) %>% 
            paste(paste('"', names(.) %>% str_to_title(), '"', sep = ""), ., sep = ":", collapse = ",") %>% 
            str_replace_all(pattern = " ", replacement = "") %>% 
            str_replace(pattern = "WorkRate", replacement = "Workrate"),
          '},"PhysicalAttributes":{',
          sapply(
            temp %>% 
              select(`Acceleration`:`Strength`),
            FUN = function(x) {if_else(is.na(x), 5, x)}
          ) %>% 
            paste(paste('"', names(.) %>% str_to_title(), '"', sep = ""), ., sep = ":", collapse = ",") %>% 
            str_replace_all(pattern = " ", replacement = "") %>% 
            str_replace(pattern = "JumpingReach", replacement = "Jumping"),
          '},"TechnicalAttributes":{',
          sapply(
            temp %>% 
              select(`Corners`:`Technique`),
            FUN = function(x) {if_else(is.na(x), 5, x)}
          ) %>% 
            paste(paste('"', names(.) %>% str_to_title(), '"', sep = ""), ., sep = ":", collapse = ",") %>% 
            str_replace_all(pattern = " ", replacement = "") %>% 
            str_replace(pattern = "FreeKick", replacement = "Freekicks") %>% 
            str_replace(pattern = "LongThrows", replacement = "Longthrows"),
          '},"Positions":{',
          sapply(
            temp %>% 
              select(`Striker`:`Goalkeeper`),
            FUN = function(x) {if_else(is.na(x)|x == 0, 1, x)}
          ) %>% 
            paste(paste('"', names(.) %>% str_to_title(), '"', sep = ""), ., sep = ":", collapse = ",") %>% 
            str_replace_all(pattern = " ", replacement = "") %>% 
            str_replace_all(pattern = "AttackingMidfielder", replacement = "AttackingMid") %>% 
            str_replace_all(pattern = "Defense", replacement = "Defender") %>% 
            str_replace_all(pattern = "\\[L\\]", replacement = "Left") %>% 
            str_replace_all(pattern = "\\[C\\]", replacement = "Central") %>% 
            str_replace_all(pattern = "\\[R\\]", replacement = "Right") %>% 
            str_replace(pattern = "DefensiveMidfielderCentral", replacement = "DefensiveMidfielder") %>% 
            str_replace_all(pattern = "Wingback", replacement = "WingBack"),
          '},"DocumentType":"Player"}',
          sep = ""
        )
      })
      
      output$playerExport <- renderText({
        selectedData()
      })
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste(input$player, " Build.json", sep = "")
        },
        content = function(file) {
          writeLines(
            selectedData(),
            file
          )
          
        },
        contentType = "json"
      )
      
      output$playerBio <- renderReactable({
        filterName <- input$player
        
        temp <- 
          tbl(con, "Daily_Scrape") %>% 
          filter(
            Name == filterName
          ) %>% 
          collect()
        
        temp %>% 
          select(
            Class,
            Birthplace,
            Height,
            Weight,
            `Preferred Foot`,
            `Hair Color`,
            `Hair Length`,
            `Skin Tone`,
            `All Traits`
          ) %>% 
          t() %>% 
          as.data.frame() %>% 
          tibble::rownames_to_column() %>% 
          reactable()
      })      
       
    }   
  )
}

