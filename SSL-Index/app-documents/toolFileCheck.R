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
            -"Inf",
            -"Rec"
          ) %>% 
          mutate(
            Name = 
              case_when(
                str_detect(Name, "GFuel") ~ "A Singular Tub of FazeBerry ® GFuel ® Energy Formula - The Official Drink of ESports ®", 
                str_detect(Name, "Liang") ~ "Kuai Liang",
                str_detect(Name, "Princess") ~ "Princess Changshan",
                TRUE ~ Name)
          ) %>% 
          relocate(
            c(Pun, Ref, TRO),
            .after = `1v1`
          )
        
        colnames(FMAttributes) <- 
          c(
            "Name",
            # Attributes
            playerData %>% 
              select(
                Acceleration:Throwing
              ) %>% 
              colnames(),
            # Hidden traits
            "Versatility", "Temperament", "Sportmanship", "Important Matches", "Proffessionalism", "Pressure", "Loyalty", "Injury Proneness", 
            "Dirtiness", "Controversy", "Consistency", "Adaptability", "Ambition" 
          )
        
        audit <- 
          playerData %>% 
          filter(
            !(Team %in% c("FA", "Retired"))
          ) %>% 
          select(
            Name,
            Acceleration:Throwing
          ) %>% 
          left_join(
            FMAttributes,
            by = "Name", 
            suffix = c(".Forum", ".FM")
          ) %>% 
          relocate(
            sort(colnames(.))
          ) %>% 
          relocate(
            contains("Name"),
            .before = "Acceleration.FM"
          ) %>% 
          relocate(
            contains("."),
            .after = "Name"
          )
        
        comparison <- 
          comparedf(
            FMAttributes %>% 
              arrange(Name) %>% 
              select(
                Name,
                Acceleration:Throwing
              ) %>% 
              dplyr::mutate(
                across(
                  Acceleration:Throwing,
                  ~ as.numeric(.x))
              ),
            playerData %>% 
              filter(
                !(Team %in% c("FA", "Retired"))
              ) %>% 
              arrange(Name) %>% 
              select(
                Name,
                Acceleration:Throwing
              )%>% 
              dplyr::mutate(
                across(
                  Acceleration:Throwing,
                  ~ as.numeric(.x))
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
      
      
      
    }
  )
}