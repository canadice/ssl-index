exportBuildUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 4,
             p(
               "The tool packages all updated builds from the last week into a zip-file. Each player is summarized in their own
               JSON file which can be imported using FMRTE. NOTE THAT NATIONALITIES ARE NOT INCLUDED IN THE JSON."
             )
           ),
      column(
        width =4, 
        div(
          id = ns("playerDownload"),
          downloadButton(ns("downloadData"), "Download Updated Players")
        )
      )
    ),
    fluidRow(
      column(width = 12,
             reactableOutput(ns("changes"))
           )
    ),
    br(),
    fluidRow(
      column(width = 6,
             h4("Select a player and download a single build:"),
             uiOutput(ns("singles")) %>% 
               withSpinnerSmall(),
             ),
      column(
        width =4, 
        uiOutput(ns("singleNationality")),
        div(
          id = ns("singlePlayerDownload"),
          downloadButton(ns("singleDownloadData"), "Download Selected Player")
        )
      )
    )
  )
}

exportBuildServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      builds <- reactive({
        getChangedBuilds()
      })
      
      build <- reactive({
        pid <- getPlayerID(input$selectedPlayer)
        
        getPlayerDataAsync(pid = pid$pid)      
      })
      
      downloadPlayer <- function(temp){
        traits <- 
          temp$traits %>% 
          str_split(pattern = " \\\\ ", simplify = TRUE) %>% 
          str_to_lower() %>% 
          {
            (jsonTraits %>% str_to_lower()) %in% .  
          } %>% 
          which() %>% 
          {
            sum(2^(.-1))
          }
        
        paste(
          '{"GoalKeeperAttributes":{
          ',
          sapply(
            temp %>% 
              select(`aerial reach`:throwing),
            FUN = function(x) {if_else(is.na(x), 5, x)}
          ) %>% 
            paste(paste('"', names(.) %>% str_to_title(), '"', sep = ""), ., sep = ":", collapse = ",") %>% 
            str_replace_all(pattern = " ", replacement = "") %>% 
            str_replace(pattern = "TendencyToRush", replacement = "RushingOut") %>% 
            str_replace(pattern = "AerialReach", replacement = "AerialAbility"),
          '},
"MentalAttributes":{
          ',
          sapply(
            temp %>% 
              select(`aggression`:`work rate`),
            FUN = function(x) {if_else(is.na(x), 5, x)}
          ) %>% 
            paste(paste('"', names(.) %>% str_to_title(), '"', sep = ""), ., sep = ":", collapse = ",") %>% 
            str_replace_all(pattern = " ", replacement = "") %>% 
            str_replace(pattern = "WorkRate", replacement = "Workrate"),
          '},
"PhysicalAttributes":{
          ',
          sapply(
            temp %>% 
              select(`acceleration`:`strength`),
            FUN = function(x) {if_else(is.na(x), 5, x)}
          ) %>% 
            paste(paste('"', names(.) %>% str_to_title(), '"', sep = ""), ., sep = ":", collapse = ",") %>% 
            str_replace_all(pattern = " ", replacement = "") %>% 
            str_replace(pattern = "JumpingReach", replacement = "Jumping"),
          ',"LeftFoot":', temp$`left foot`,
          ',"RightFoot":', temp$`right foot`,
          '},
"TechnicalAttributes":{
          ',
          sapply(
            temp %>% 
              select(`corners`:`technique`),
            FUN = function(x) {if_else(is.na(x), 5, x)}
          ) %>% 
            paste(paste('"', names(.) %>% str_to_title(), '"', sep = ""), ., sep = ":", collapse = ",") %>% 
            str_replace_all(pattern = " ", replacement = "") %>% 
            str_replace(pattern = "FreeKick", replacement = "Freekicks") %>% 
            str_replace(pattern = "LongThrows", replacement = "Longthrows"),
          '},
"Positions":{
          ',
          sapply(
            temp %>% 
              select(`pos_st`:`pos_gk`),
            FUN = function(x) {if_else(is.na(x)|x == 0, 1, as.numeric(x))}
          ) %>% 
            paste(paste('"', positionsGK[sapply((names(.) %>% str_remove_all(pattern = "pos_") %>% str_to_upper()), FUN = function(x) which(names(positionsGK) == x))], '"', sep = ""), ., sep = ":", collapse = ",") %>% 
            str_replace_all(pattern = " ", replacement = "") %>% 
            str_replace_all(pattern = "AttackingMidfielder", replacement = "AttackingMid") %>% 
            str_replace_all(pattern = "Wingback", replacement = "WingBack") %>% 
            str_replace_all(pattern = "(Left|Right|Central)([A-Za-z]+)(\":\"?[0-9]+)", replacement = "\\2\\1\\3"),
          '},
"HairColour":"', hairColor[temp$hair_color] %>% names() %>% str_replace_all(pattern = " |\\(|\\)", replacement = ""), 
          '","HairLength":"', temp$hair_length %>% str_to_title(), 
          '","SkinColour":', temp$skintone, 
          ',"Height":', 
          if_else(
            !is.na(temp$height %>% as.numeric()), 
            (temp$height %>% as.numeric()*2.54) %>% as.character(), 
            temp$height %>% as.character()
          ),
          ',"Weight":', 
          if_else(
            !is.na(temp$weight %>% as.numeric()), 
            (temp$weight %>% as.numeric()*0.453592) %>% as.character(), 
            temp$weight %>% as.character()
          ),
          ',"PreferredMoves":', traits,
          ',"Born":"', 
          ("2004-07-01" %>% as_date()) - years(currentSeason$season - (temp$class %>% str_extract(pattern = "[0-9]+") %>% as.numeric())) ,
          '","DocumentType":"Player"}',
          sep = ""
        )
      }
      
      #### ALL CHANGED BUILDS ####
      output$changes <- renderReactable({
         builds() %>% 
          then(
            onFulfilled = function(data){
              data %>% 
                select(
                  Name = name,
                  Team = team,
                  Nationality = nationality
                ) %>% 
                reactable()
            }
          )
      })
      
      output$downloadData <- downloadHandler(
        filename = function(name) {
          paste(lubridate::today(), "Builds.zip", sep = "")
        },
        content = function(file){
          builds() %>% 
            then(
              onFulfilled = function(data){
                temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
                dir.create(temp_directory)
                
                data$name %>%
                  unique() %>% 
                  imap(function(x,y){
                    if(!is.null(x)){
                      file_name <- glue::glue("{x} Build.json")
                      writeLines(
                        downloadPlayer(data %>% filter(name == x)),
                        file.path(temp_directory, file_name)
                      )
                    }
                  })
                
                zip::zip(
                  zipfile = file,
                  files = dir(temp_directory),
                  root = temp_directory
                )
              }
            )
        },
        contentType = "application/zip"
      )
      
      #### SINGLE PLAYER BUILD ####
      output$singles <- renderUI({
        getPlayersFromAllTeams() %>% 
          then(
            onFulfilled = function(data){
              selectizeInput(
                inputId = session$ns("selectedPlayer"), 
                label = "Select a player to export", 
                choices = c(data$name)
              )      
            }
          )
      })
      
      output$singleNationality <- renderUI({
        req(input$selectedPlayer)
        build() %>% 
          then(
            onFulfilled = function(data){
              tagList(
                h4("Position: ", data$position),
                h4("Nationality: ", data$nationality)
              )
            }
          )
      })
      
      output$singleDownloadData <- downloadHandler(
        filename = function(name) {
          paste(input$selectedPlayer, " Build.json", sep = "")
        },
        content = function(file){
          build() %>% 
            then(
              onFulfilled = function(data){
                writeLines(
                  downloadPlayer(data),
                  file
                )
              }
            )
        },
        contentType = "json"
      )
    }
  )
}