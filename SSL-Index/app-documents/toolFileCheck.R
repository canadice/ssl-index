fileCheckUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 2,
        fileInput(
          inputId = ns("fm"),
          label = "Upload the exported view",
          accept = ".html"
        )
      ),
      column(
        width =4, 
        div(
          id = ns("playerDownload"),
          downloadButton(ns("downloadData"), "Download Wrong Players")
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        reactableOutput(
          outputId = ns("attributes")
        ) 
      )
    )
  )
}

fileCheckServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      auditFunction <- function(path) {
        FMAttributes <- 
          read_html(path, encoding = "UTF-8") %>% 
          html_elements("table") %>% 
          html_table() %>% 
          .[[1]] %>% 
          mutate(
            Name = 
              Name %>% 
              str_split(
                pattern = " - ", 
                simplify = TRUE
              ) %>% 
              .[,1]
          ) %>% 
          select(
            Name,
            Acc:`Right Foot`
          ) %>% 
          mutate(
            Name = 
              case_when(
                str_detect(Name, "GFuel") ~ "A Singular Tub of FazeBerry ® GFuel ® Energy Formula - The Official Drink of ESports ®", 
                str_detect(Name, "Liang") ~ "Kuai Liang",
                str_detect(Name, "Princess") ~ "Princess Changshan",
                TRUE ~ Name),
            `Left Foot` = case_when(`Left Foot` == "Very Strong" ~ 20,
                                    `Left Foot` == "Reasonable" ~ 10,
                                    TRUE ~ 15),
            `Right Foot` = case_when(`Right Foot` == "Very Strong" ~ 20,
                                     `Right Foot` == "Reasonable" ~ 10,
                                     TRUE ~ 15)
          ) %>% 
          relocate(
            c(Pun, Ref, TRO),
            .after = `1v1`
          )
        
        colnames(FMAttributes) <- 
          c(
            "Name",
            sapply(
              colnames(FMAttributes), 
              FUN = function(x) attributes$Attribute[attributes$abbr == x]
            ) %>% unlist() %>% stringi::stri_remove_empty_na(),
            "Left Foot", "Right Foot"
            # # Hidden traits
            # "Versatility", "Temperament", "Sportmanship", "Important Matches", "Proffessionalism", "Pressure", "Loyalty", "Injury Proneness", 
            # "Dirtiness", "Controversy", "Consistency", "Adaptability", "Ambition" 
          )
        
        audit <- 
          playerData %>% 
          filter(
            !(Team %in% c("FA", "Retired"))
          ) %>% 
          select(
            Name,
            Acceleration:Throwing,
            `Preferred Foot`
          ) %>% 
          mutate(
            `Left Foot` = 
              case_when(`Preferred Foot` == "Left" ~ 20, 
                        `Preferred Foot` == "Right" ~ 10,
                        TRUE ~ 
                          `Preferred Foot` %>% 
                            str_split(pattern = " \\| ", simplify = TRUE) %>% 
                            .[,1] %>% 
                            as.numeric()
                      ),
            `Right Foot` = 
              case_when(`Preferred Foot` == "Right" ~ 20, 
                        `Preferred Foot` == "Left" ~ 10,
                        TRUE ~ 
                          `Preferred Foot` %>% 
                            str_split(pattern = " \\| ", simplify = TRUE) %>% 
                            .[,2] %>% 
                            as.numeric()
              )
          ) %>% 
          select(
            !`Preferred Foot`
          # ) %>%
          # left_join(
          #   FMAttributes,
          #   by = "Name",
          #   suffix = c(".Forum", ".FM")
          # ) %>%
          # relocate(
          #   sort(colnames(.))
          # ) %>%
          # relocate(
          #   contains("Name"),
          #   .before = "Acceleration.FM"
          # ) %>%
          # relocate(
          #   contains("."),
          #   .after = "Name"
          )
        
        comparison <- 
          comparedf(
            FMAttributes %>% 
              arrange(Name) %>% 
              select(
                Name,
                Acceleration:`Right Foot`
              ) %>% 
              dplyr::mutate(
                across(
                  Acceleration:`Right Foot`,
                  ~ as.numeric(.x) %>% 
                    replace_na(5))
              ),
            audit %>% 
              dplyr::mutate(
                across(
                  Acceleration:`Right Foot`,
                  ~ as.numeric(.x) %>% 
                    replace_na(5))
              ),
            by = "Name"
          ) 
        
        auditAttributes <- 
          comparison %>% 
          summary() %>% 
          .$diffs.table %>% 
          arrange(Name) %>% 
          filter(
            !is.na(values.y)
          )
        
        colnames(auditAttributes) <- 
          str_replace(
            colnames(auditAttributes),
            ".x",
            ".FM"
          )
        
        colnames(auditAttributes) <- 
          str_replace(
            colnames(auditAttributes),
            ".y",
            ".Forum"
          )
        
        
        auditPlayers <- 
          comparison %>% 
          summary() %>% 
          .$obs.table %>% 
          arrange(Name) %>% 
          mutate(
            version = if_else(version == "x", "FM", "Forum")
          )
        
        list(
          "Attributes" = auditAttributes,
          "Players" = auditPlayers
        ) %>% 
          return()
      }
      
      filePath <- reactive({
        file <- input$fm
        
        file$datapath
      })
      
      output$attributes <- renderReactable({
        
        req(filePath())
        list <- 
          auditFunction(filePath())
        
        list$Attributes %>% 
          select(
            -c(row.FM, row.Forum)
          ) %>% 
          reactable(
            pagination = TRUE,
            defaultPageSize = 50,
            paginationType = "numbers",
            theme = pff(font_color = "#000"),
            searchable = TRUE
          )
        
      })
      
      downloadPlayer <- function(name){
        con <- 
          dbConnect(
            SQLite(), 
            dbFile
          )
        
        temp <- 
          tbl(con, "Daily_Scrape") %>% 
          filter(
            Name == name
          ) %>% 
          collect()
        
        dbDisconnect(con)
        
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
          ',"LeftFoot":', 
          if_else(temp$`Preferred Foot` %>% str_detect("\\|"), 
                  temp$`Preferred Foot` %>% 
                    str_split(" \\| ", simplify = TRUE) %>% 
                    .[1],
                  if_else(
                    temp$`Preferred Foot` == "Left", 
                    "20", 
                    "10"
                  )
          ),
          ',"RightFoot":', 
          if_else(temp$`Preferred Foot` %>% str_detect("\\|"), 
                  temp$`Preferred Foot` %>% 
                    str_split(" \\| ", simplify = TRUE) %>% 
                    .[2],
                  if_else(
                    temp$`Preferred Foot` == "Left", 
                    "10", 
                    "20"
                  )
          ),
          '}', 
          ',"TechnicalAttributes":{',
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
            FUN = function(x) {if_else(is.na(x)|x == 0, 1, as.numeric(x))}
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
          '}',
          ',"HairColour":"', temp$`Hair Color` %>% str_replace_all(pattern = " |\\(|\\)", replacement = ""), 
          '","HairLength":"', temp$`Hair Length` %>% str_to_title(), 
          '","SkinColour":', temp$`Skin Tone` %>% str_extract(pattern = "[0-9]+"), 
          ',"Height":', 
          if_else(
            !is.na(temp$Height %>% as.numeric()), 
            (temp$Height %>% as.numeric()*2.54) %>% as.character(), 
            temp$Height
          ),
          ',"Weight":', 
          if_else(
            !is.na(temp$Weight %>% as.numeric()), 
            (temp$Weight %>% as.numeric()*0.453592) %>% as.character(), 
            temp$Weight
          ),
          ',"Born":"', 
          "2004-07-01" %>% as_date() - years(12 - (temp$Class %>% str_extract(pattern = "[0-9]+") %>% as.numeric())) ,
          '","DocumentType":"Player"}',
          sep = ""
        )
      }
      
      output$downloadData <- downloadHandler(
        filename = function(name) {
          paste(lubridate::today(), "Builds.zip", sep = "")
        },
        content = function(file){
          
          temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
          dir.create(temp_directory)
          
          req(filePath())
          list <- 
            auditFunction(filePath()) 
          
          list$Attributes$Name %>%
            unique() %>% 
            imap(function(x,y){
              if(!is.null(x)){
                file_name <- glue::glue("{x} Build.json")
                writeLines(
                  downloadPlayer(x),
                  file.path(temp_directory, file_name)
                )
              }
            })
          
          zip::zip(
            zipfile = file,
            files = dir(temp_directory),
            root = temp_directory
          )
        },
        contentType = "application/zip"
      )
      
    }
  )
}