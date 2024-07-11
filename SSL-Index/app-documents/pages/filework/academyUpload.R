academyUploadUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Upload and Update Academy Season Statistics"),
    p(
      paste(
        "Export the Player Statistics for Export view from the League Observer Manager shortlist after the matchday you want to upload.",
        "In case you do not have the view for the League Observer Manager, download and import the view via", 
        tags$a("this link.", href = "https://drive.google.com/open?id=1b4yz5gkXN6BFSvDBigL3Tp2pUBbu-PJ3&usp=drive_fs", target = "_blank"),
        "The view should be exported as an .HTML file and make sure to scroll through every player in the shortlist as FM only exports players it has 'seen'."
        ) %>% HTML()
    ),
    fluidRow(
      column(
        width = 4,
        fileInput(
          inputId = ns("fm"),
          label = "Upload the exported view ",
          accept = ".html"
        ),
        selectInput(
          inputId = ns("season"),
          label = "Which season is the view from?",
          choices = 1:currentSeason$season,
          selected = currentSeason$season
        )
      )
    ),
    fluidRow(
      column(
        width = 8,
        h5("Keeper"),
        reactableOutput(
          outputId = ns("keeperCheck")
        ) %>% 
          withSpinnerSmall(),
        h5("Outfield"),
        reactableOutput(
          outputId = ns("outfieldCheck")
        ) %>% 
          withSpinnerSmall()
      )
    ),
    fluidRow(
      class = "frozen-bottom",
      column(width = 12,
             actionButton(inputId = ns("uploadData"), label = "Upload game")
             )
    ),
    div(style = "min-height:100px;")
  )
}
academyUploadServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      #### REACTIVES ####
      filePath <- reactive({
        file <- input$fm
        
        file$datapath
      })
      
      processedGame <- reactive({
        req(filePath())
        
        current <- fmData(filePath())
        
        splitKeeper <- 
          current %>%
          filter(position == "GK", !is.na(`minutes played`)) %>% 
          select(name:`minutes played`, `average rating`:`player of the match`, won:`xg prevented`) %>% 
          mutate(season = input$season) %>% 
          relocate(season) %>% 
          mutate(
            across(
              !(name:position),
              ~ replace_na(.x, 0)
            )
          )
          
        
        splitOutfield <- 
          current %>% 
          filter(!is.na(`minutes played`)) %>% 
          select(name:`attempted presses`) %>% 
          mutate(season = input$season) %>% 
          relocate(season) %>% 
          mutate(
            across(
              !(name:position),
              ~ replace_na(.x, 0)
            )
          )
        
        list(
          k = splitKeeper,
          o = splitOutfield
        )
        
      })
      
      #### FUNCTIONS ####
      fmData <- function(path){
        FM <- 
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
            ## FM24 does not have Rec as an automatic column
            # -"Rec"
          ) %>% 
          relocate(
            c(Pun, Ref, TRO),
            .after = `1v1`
          ) %>% 
          dplyr::rename(
            Apps = Apps,
            `Minutes Played` = Mins,
            `Distance Run (km)` = Distance,
            `Average Rating` = `Av Rat`,
            `Player of the Match` = `PoM`,
            Goals = Gls,
            Assists = Ast,
            `xG Overperformance` = `xG-OP`,
            
            `Shots on Target` = ShT,
            
            `Blocks` = Blk,
            
            `Penalties Taken` = Pens,
            `Penalties Scored` = `Pens S`,
            
            `Attempted Passes` = `Pas A`,
            `Successful Passes` = `Ps C`,
            `Key Passes` = `K Pas`,
            `Open Play Key Passes` = `OP-KP`,
            
            `Successful Open Play Crosses` = `OP-Crs C`,
            `Attempted Open Play Crosses` = `OP-Crs A`,
            `Successful Crosses` = `Cr C`,
            `Attempted Crosses` = `Cr A`,
            
            `Chances Created` = CCC,
            
            `Successful Headers` = Hdrs,
            `Attempted Headers` = `Hdrs A`,
            `Header%` = `Hdr %`,
            `Key Headers` = `K Hdrs`,
            
            Dribbles = `Drb`,
            
            `Attempted Tackles` = `Tck A`,
            `Tackles Won` = `Tck C`,
            `Tackle%` = `Tck R`,
            `Key Tackles` = `K Tck`,
            
            Interceptions = Itc,
            `Shots Blocked` = `Shts Blckd`,
            Clearances = Clear,
            `Mistakes Leading to Goals` = `Gl Mst`,
            `Yellow Cards` = Yel,
            `Red Cards` = Red,
            Fouls = Fls,
            `Fouls Against` = FA,
            Offsides = Off,
            
            `Progressive Passes` = `Pr Passes`,
            
            `Successful Presses` = `Pres C`,
            `Attempted Presses` = `Pres A`,
            
            Drawn = D,
            Conceded = Conc,
            `Saves Parried` = Svp,
            `Saves Held`= Svh,
            `Saves Tipped` = Svt,
            `Penalties Saved` = `Pens Saved`,
            `Penalties Faced` = `Pens Faced`,
            # `Clean Sheets` = `Clean sheets`
            `Clean Sheets` = Shutouts,
            `xSave%`= `xSv %`,
            `xG Prevented` = xGP
          ) %>% 
          dplyr::mutate(
            across(
              c(
                `Minutes Played`:`Wor`,
                `Won`:`xG Prevented`
              ),
              .fns = str_replace_all,
              pattern = "[^-\\d\\.]+",
              replacement = ""
            )
          ) %>% 
          dplyr::mutate(
            Club =
              Club %>%
              str_split(pattern = "-", simplify = TRUE) %>% 
              .[,1] %>% 
              str_squish(),
            `Left Foot` = 
              case_when(
                `Left Foot` == "Very Strong" ~ 20,
                `Left Foot` == "Strong" ~ 15,
                TRUE ~ 10
              ),
            `Right Foot` = 
              case_when(
                `Right Foot` == "Very Strong" ~ 20,
                `Right Foot` == "Strong" ~ 15,
                TRUE ~ 10
              )
          ) %>% 
          dplyr::mutate(
            across(
              !contains(
                c("Name", "Information", "Nationality", "Position", "Club")
              ),
              as.numeric
            ),
            `Pass%` = 
              (`Successful Passes` %>% as.numeric()/
                 `Attempted Passes` %>% as.numeric()) %>% 
              round(4)*100,
            `Cross%` = 
              (`Successful Crosses` %>% as.numeric()/
                 `Attempted Crosses` %>% as.numeric()) %>% 
              round(4)*100,
            `Save%` = 
              ((`Saves Parried`+`Saves Held`+`Saves Tipped`)/
                 (`Saves Parried`+`Saves Held`+`Saves Tipped`+Conceded)) %>% 
              round(4) * 100,
          ) %>% 
          rename_with(
            ~ str_to_lower(.x)
          ) %>% 
          relocate(
            `pass%`, .after = `attempted passes`
          ) %>% 
          relocate(
            `cross%`, .after = `attempted crosses`
          ) %>% 
          relocate(
            `save%`, .after = `saves tipped`
          ) %>% 
          suppressWarnings()
      }
      
      #### OUTPUTS ####
      output$keeperCheck <- renderReactable({
        req(filePath())
        reactable(
          processedGame()$k
        )
      })
      
      output$outfieldCheck <- renderReactable({
        req(filePath())
        reactable(
          processedGame()$o
        )
      })
      
      #### OBSERVERS ####
      observe({
        req(filePath())
        
        keeper <- processedGame()$k %>% 
          mutate(
            across(
              name:position,
              ~ paste0('"', .x, '"')
            )
          )
        
        outfield <- processedGame()$o %>% 
          mutate(
            across(
              name:position,
              ~ paste0('"', .x, '"')
            )
          ) 
        
        indexQuery(
          paste(
            "DELETE FROM academyoutfield WHERE season = ", input$season, ";"
          )
        )
        
        indexQuery(
          paste(
            "INSERT INTO academyoutfield VALUES ",
            do.call(function(...) paste(..., sep = ", "), args = outfield) %>% paste0("(", ., ")", collapse = ", "),
            ";"
          )
        )
        
        indexQuery(
          paste(
            "DELETE FROM academykeeper WHERE season = ", input$season, ";"
          )
        )
        
        indexQuery(
          paste(
            "INSERT INTO academykeeper VALUES ",
            do.call(function(...) paste(..., sep = ", "), args = keeper) %>% paste0("(", ., ")", collapse = ", "),
            ";"
          )
        )
        
        showToast("success", "You have successfully uploaded and replaced the Academy statistics!")
        
      }) %>% 
        bindEvent(
          input$uploadData
        )
      
    }
  )
}