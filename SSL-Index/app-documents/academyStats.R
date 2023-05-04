academyStatsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        h1("Academy Statistics"),
        tabPanel(
          "Player Stats",
          div(
            id = ns("playerDownload"),
            downloadButton(ns("downloadData"), "Download")
          ),
          h4("Outfield", align = "center"),
          reactableOutput(
            outputId = ns("playerStats")
          ),
          h4("Goalkeeper", align = "center"),
          reactableOutput(
            outputId = ns("goalieStats")
          )
        )
      )
    )
  )
}

academyStatsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      con <- 
        dbConnect(
          SQLite(), 
          dbFile
        )
      
      outfieldStats <- 
        tbl(con, "academyOutfield") %>% 
        select(-(Acc:Wor)) %>% 
        collect()
      
      goalieStats <- 
        tbl(con, "academyGoalie") %>% 
        collect()
      
      dbDisconnect(con)
      
      output$playerStats <- renderReactable({
        outfieldStats %>% 
          arrange(
            `Average Rating` %>% desc()
          ) %>% 
          reactable(
            pagination = TRUE,
            defaultPageSize = 10,
            paginationType = "numbers",
            theme = pff(),
            searchable = TRUE,
            columns = 
              list(
                Name = colDef(
                  minWidth = 250,
                  style = list(position = "sticky", left = 0, background = "#F8F8F8", zIndex = 1),
                  headerStyle = list(position = "sticky", left = 0, zIndex = 1),
                  cell = 
                    function(value, index){
                      Nation <- outfieldStats %>% 
                        arrange(
                        `Average Rating` %>% desc()
                        ) %>%  
                        .$Nationality %>% 
                        .[index]
                      
                      tagList(
                        div(value),
                        div(style = "font-size: 1rem", Nation)
                      )
                    }
                ),
                Club = 
                  colDef(
                    maxWidth = 50,
                    align = "center",
                    class = "cell",
                    cell = function(value){
                      image <- img(src = sprintf("%s.png", value), style = "height: 25px;", alt = value)
                      tagList(
                        div(style = "display: inline-block; width: 25px;", image)
                      )
                    }
                  ),
                Nationality = colDef(show = FALSE)
              )
          )
      })
      
      output$goalieStats <- renderReactable({
        goalieStats %>% 
          arrange(
            `Average Rating` %>% desc()
          ) %>% 
          reactable(
            pagination = TRUE,
            defaultPageSize = 10,
            paginationType = "numbers",
            theme = pff(),
            searchable = TRUE,
            columns = 
              list(
                Name = colDef(
                  minWidth = 250,
                  style = list(position = "sticky", left = 0, background = "#F8F8F8",zIndex = 1),
                  headerStyle = list(position = "sticky", left = 0, zIndex = 1),
                  cell = 
                    function(value, index){
                      Nation <- goalieStats %>% 
                        arrange(
                          `Average Rating` %>% desc()
                        ) %>%  
                        .$Nationality %>% 
                        .[index]
                      
                      tagList(
                        div(value),
                        div(style = "font-size: 1rem", Nation)
                      )
                    }
                ),
                Club = 
                  colDef(
                    maxWidth = 50,
                    align = "center",
                    class = "cell",
                    cell = function(value){
                      image <- img(src = sprintf("%s.png", value), style = "height: 25px;", alt = value)
                      tagList(
                        div(style = "display: inline-block; width: 25px;", image)
                      )
                    }
                  ),
                Nationality = colDef(show = FALSE)
              )
          )
      })
    }
  )
}