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
          '{"GoalKeeperAttributes":{',
          sapply(
            temp %>% 
              select(`aerial reach`:throwing),
            FUN = function(x) {if_else(is.na(x), 5, x)}
          ) %>% 
            paste(paste('"', names(.) %>% str_to_title(), '"', sep = ""), ., sep = ":", collapse = ",") %>% 
            str_replace_all(pattern = " ", replacement = "") %>% 
            str_replace(pattern = "TendencyToRush", replacement = "RushingOut") %>% 
            str_replace(pattern = "AerialReach", replacement = "AerialAbility"),
          '},"MentalAttributes":{',
          sapply(
            temp %>% 
              select(`aggression`:`work rate`),
            FUN = function(x) {if_else(is.na(x), 5, x)}
          ) %>% 
            paste(paste('"', names(.) %>% str_to_title(), '"', sep = ""), ., sep = ":", collapse = ",") %>% 
            str_replace_all(pattern = " ", replacement = "") %>% 
            str_replace(pattern = "WorkRate", replacement = "Workrate"),
          '},"PhysicalAttributes":{',
          sapply(
            temp %>% 
              select(`acceleration`:`strength`),
            FUN = function(x) {if_else(is.na(x), 5, x)}
          ) %>% 
            paste(paste('"', names(.) %>% str_to_title(), '"', sep = ""), ., sep = ":", collapse = ",") %>% 
            str_replace_all(pattern = " ", replacement = "") %>% 
            str_replace(pattern = "JumpingReach", replacement = "Jumping"),
          ',"LeftFoot":', 
          if_else(temp$footedness %>% str_detect("\\|"), 
                  temp$footedness %>% 
                    str_split(" \\| ", simplify = TRUE) %>% 
                    .[1],
                  if_else(
                    temp$footedness == "Left", 
                    "20", 
                    "10"
                  )
          ),
          ',"RightFoot":', 
          if_else(temp$footedness %>% str_detect("\\|"), 
                  temp$footedness %>% 
                    str_split(" \\| ", simplify = TRUE) %>% 
                    .[2],
                  if_else(
                    temp$footedness == "Left", 
                    "10", 
                    "20"
                  )
          ),
          '}', 
          ',"TechnicalAttributes":{',
          sapply(
            temp %>% 
              select(`corners`:`technique`),
            FUN = function(x) {if_else(is.na(x), 5, x)}
          ) %>% 
            paste(paste('"', names(.) %>% str_to_title(), '"', sep = ""), ., sep = ":", collapse = ",") %>% 
            str_replace_all(pattern = " ", replacement = "") %>% 
            str_replace(pattern = "FreeKick", replacement = "Freekicks") %>% 
            str_replace(pattern = "LongThrows", replacement = "Longthrows"),
          '},"Positions":{',
          sapply(
            temp %>% 
              select(`pos_st`:`pos_gk`),
            FUN = function(x) {if_else(is.na(x)|x == 0, 1, as.numeric(x))}
          ) %>% 
            paste(paste('"', positionsGK[names(positionsGK) %in% (names(.) %>% str_remove_all(pattern = "pos_") %>% str_to_upper())], '"', sep = ""), ., sep = ":", collapse = ",") %>% 
            str_replace_all(pattern = " ", replacement = "") %>% 
            str_replace_all(pattern = "AttackingMidfielder", replacement = "AttackingMid") %>% 
            str_replace_all(pattern = "Wingback", replacement = "WingBack"),
          '}',
          ',"HairColour":"', temp$hair_color %>% str_replace_all(pattern = " |\\(|\\)", replacement = ""), 
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
          "2004-07-01" %>% as_date() - years(currentSeason$season - (temp$class %>% str_extract(pattern = "[0-9]+") %>% as.numeric())) ,
          '","DocumentType":"Player"}',
          sep = ""
        )
      }
      
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
    }
  )
}