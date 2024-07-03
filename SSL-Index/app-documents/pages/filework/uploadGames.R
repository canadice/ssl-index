uploadGameUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Upload and Update Game Statistics"),
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
        p("If the data is correctly imported the table should show 90 minutes on average for outfield and keeper data. Red cards or injuries might affect these results so check that before submitting."),
        h5("Checks"),
        reactableOutput(
          outputId = ns("outputMinutes")
        ) %>% 
          withSpinnerSmall(),
        h5("Players per Team"),
        reactableOutput(
          outputId = ns("outputPlayers")
        ) %>% 
          withSpinnerSmall()
      )
    )
  )
}
uploadGameServer <- function(id) {
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
        
        keeperTotals <- getKeeperSeasonTotal(input$season)
        outfieldTotals <- getOutfieldSeasonTotal(input$season)
        
        current <- 
          current %>% 
          rename_with(
            .cols = !c(Acc:`Right Foot`, Position, Won, Lost, Drawn),
            .fn = function(x){
              intersect(x %>% str_to_lower(),
                        c(colnames(outfieldTotals), colnames(keeperTotals)) %>% 
                          unique())
            } 
          ) %>% 
          relocate(
            c(colnames(outfieldTotals), colnames(keeperTotals)) %>% 
              unique()
          )
        
        splitKeeper <- 
          current %>%
          filter(Position == "GK") %>% 
          select(colnames(keeperTotals)) %>% 
          full_join(
            keeperTotals,
            by = c("name", "club")
          ) %>% 
          group_by(
            name, club
          ) %>% 
          mutate(
            across2(
              .xcols = ends_with(".x"),
              .ycols = ends_with(".y"),
              .fns = 
                list(
                  diff = ~ sum(.x, -.y, na.rm = TRUE)
                ),
              .names = "{xcol}"
            )
          ) %>% 
          select(
            !contains(".y")
          ) %>% 
          rename_with(
            ~ str_replace(.x, "\\.x", "")
          ) %>% 
          filter(
            `minutes played`>0
          ) %>% 
          left_join(
            keeperTotals %>% 
              select(
                name, club, `average rating`, `xsave%`, apps
              ),
            by = c("name", "club"),
            suffix = c("day", "season")
          ) %>% 
          group_by(name) %>% 
          mutate(
            `average ratingday` = 
              case_when(
                is.na(`average ratingseason`)  | `average ratingseason` == 0 ~ `average ratingday`,
                TRUE ~ (
                  (`average ratingday` + `average ratingseason`) * 
                    (`appsseason` + 1) -
                    `average ratingseason`*`appsseason`
                )
              ) %>% round(2),
            `save%` = ((`saves parried`+`saves held`+`saves tipped`)/(`saves parried`+`saves held`+`saves tipped`+conceded)) %>% round(4) * 100,
            `xsave%day` = 
              case_when(
                is.na(`xsave%season`) | `xsave%season` == 0 ~ `xsave%day`,
                TRUE ~ (
                  ((`xsave%day` + `xsave%season`) -
                     `xsave%season`/2)*2
                )
              ) %>% round(2) 
          ) %>% 
          select(
            !contains("season"),
            `average rating` = `average ratingday`,
            `xsave%` = `xsave%day`,
            `apps` = `appsday`
          ) %>% 
          mutate(
            apps = 
              case_when(
                `minutes played` > 45 ~ 1,
                TRUE ~ 0.5
              )
          ) %>%
          ungroup()
        
        splitOutfield <- 
          current %>% 
          select(colnames(outfieldTotals)) %>% 
          full_join(
            outfieldTotals,
            by = c("name", "club")
          ) %>% 
          group_by(
            name, club
          ) %>% 
          mutate(
            across2(
              .xcols = ends_with(".x"),
              .ycols = ends_with(".y"),
              .fns = 
                list(
                  diff = ~ sum(.x, -.y, na.rm = TRUE)
                ),
              .names = "{xcol}"
            )
          ) %>% 
          select(
            !contains(".y")
          ) %>% 
          rename_with(
            ~ str_replace(.x, "\\.x", "")
          ) %>% 
          filter(
            `minutes played`>0
          ) %>% 
          left_join(
            outfieldTotals %>% 
              select(name, club, `average rating`, apps),
            by = c("name", "club"),
            suffix = c("day", "season")
          ) %>% 
          group_by(name) %>% 
          mutate(
            `average ratingday` = 
              case_when(
                is.na(`average ratingseason`) | `average ratingseason` == 0 ~ `average ratingday`,
                TRUE ~ (
                  (`average ratingday` + `average ratingseason`) * 
                    (`appsseason` + 1) -
                    `average ratingseason`*`appsseason`
                )
              ) %>% round(2),
            `pass%` = (`successful passes` %>% as.numeric()/`attempted passes` %>% as.numeric()) %>% round(4)*100,
            `header%` = (`successful headers` %>% as.numeric()/`attempted headers` %>% as.numeric()) %>% round(4)*100,
            `cross%` = (`successful crosses` %>% as.numeric()/`attempted crosses` %>% as.numeric()) %>% round(4)*100,
            `tackle%` = (`tackles won` %>% as.numeric()/`attempted tackles` %>% as.numeric()) %>% round(4)*100,
            clearances = if_else(is.na(clearances), 0, clearances)
          ) %>% 
          select(
            !contains("season"),
            `average rating` = `average ratingday`,
            `apps` = `appsday`
          ) %>% 
          mutate(
            apps = 
              case_when(
                `minutes played` > 45 ~ 1,
                TRUE ~ 0.5
              )
          ) %>% 
          ungroup()
        
        list(
          splitKeeper,
          splitOutfield,
          checks = tibble(
            `Outfield Minutes` = (sum(splitOutfield$`minutes played`)/length(splitOutfield$`club` %>% unique())/11) %>% round(1),
            `Keeper Minutes` = sum(splitKeeper$`minutes played`)/length(splitKeeper$`club` %>% unique()) %>% round(1),
            `Red Cards` = splitOutfield$`red cards` %>% sum()
          ),
          playersPerTeam = 
            splitOutfield %>%
            select(name, club) %>% 
            pivot_longer(name) %>% 
            group_by(club) %>% 
            summarize(n = n())
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
          mutate(
            Name = 
              case_when(
                str_detect(Name, "GFuel") ~ "A Singular Tub of FazeBerry ® GFuel ® Energy Formula - The Official Drink of ESports ®", 
                # str_detect(Name, "Liang") ~ "Kuai Liang",
                # str_detect(Name, "Princess") ~ "Princess Changshan",
                TRUE ~ Name)
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
            ),
            Club = 
              case_when(
                Club == "Accra FC" ~ "Adowa Accra FC",
                Club == "São Paulo" ~ "União São Paulo",
                Club == "Red Star" ~ "Red Star Laos",
                Club == "E. Europe" ~ "Eastern Europe",
                Club == "Walland" ~ "Cymru",
                Club == "Reykjavik U." ~ "Reykjavik United",
                Club == "Montréal U." ~ "Montréal United",
                Club == "North Shore" ~ "North Shore United",
                Club == "Football Club de Rio" ~ "FC Rio",
                TRUE ~ Club
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
          suppressWarnings()
      }
      
      #### OUTPUTS ####
      output$outputMinutes <- renderReactable({
        req(filePath())
        reactable(
          processedGame()$checks
        )
      })
      
      output$outputPlayers <- renderReactable({
        req(filePath())
        reactable(
          processedGame()$playersPerTeam %>% 
            rename_with(
              ~ str_to_title(.x)
            )
        )
      })
      
    }
  )
}