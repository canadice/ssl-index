regressionUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        offset = 1,
        width = 10,
        box(
          status = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
          width = NULL,
          title = "Current Regression",
          DTOutput(outputId = ns("dataTable"))
        )
      )
      
    )
  )
}

regressionServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      con <- 
        dbConnect(
          SQLite(), 
          dbFile
        )
      
      currentSeason <-
        tbl(con, "gameDataPlayer") %>% 
        select(
          Season
        ) %>% 
        filter(
          Season == max(Season, na.rm = TRUE)
        ) %>% 
        collect() %>% 
        unique() %>% 
        unlist()

      
      
      regressionData <- 
        tbl(con, "Daily_Scrape") %>% 
        select(
          Username,
          Class,
          TPE,
          Team,
          Name
        ) %>% 
        filter(
          Team != "Retired"
        ) %>% 
        collect() %>% 
        group_by(Name) %>% 
        mutate(
          Age = 
            (currentSeason + 1 - 
            (str_extract(Class, pattern = "[0-9]+") %>% as.numeric())) %>% 
            unlist()
        ) %>% 
        ungroup() %>% 
        filter(
          Age > 8
        ) %>% 
        arrange(
          Team
        ) %>% 
        mutate(
          Percentage = 
            case_when(
              # Regression scale based on age
              Age < 11 ~ 0.10,
              Age < 12 ~ 0.15,
              Age < 13 ~ 0.20,
              Age < 14 ~ 0.25,
              Age < 15 ~ 0.30,
              TRUE ~ 0.40
            ),
          `Regressed TPE` = (TPE * Percentage) %>% ceiling(),
          `Remaining TPE` = TPE - `Regressed TPE`
        ) %>% 
        left_join(
          teamInfo,
          by = c("Team" = "team")
        ) %>% 
        mutate(
          Team = factor(Team) %>% relevel("FA") %>% relevel("Prospect")
        ) %>% 
        arrange(Team %>% desc())
      
      output$dataTable <- DT::renderDT({
        regressionData %>% 
          select(
            Username:`Remaining TPE`,
            color_primary,
            color_secondary
          ) %>% 
          datatable(
            escape = FALSE, 
            rownames = FALSE,
            extensions = c('Buttons', 'Scroller','RowGroup'),
            fillContainer = TRUE,
            options = 
              list(
                rowGroup = list(dataSrc = 3),
                columnDefs = 
                  list(
                    list(
                      targets = c(3, 5,9:10),
                      visible = FALSE
                    )
                  ),
                ordering = FALSE, 
                ## Sets a scroller for the rows
                scrollY = '650px',
                ## Sets size of rows shown
                scrollCollapse = TRUE,
                ## Sets width of columns
                # autoWidth = TRUE,
                ## Removes pages in the table
                paging = FALSE,
                ## Adds scrollable horizontal
                # pageLength = 20,
                # lengthMenu = c(10, 25, 50, 100),
                dom = 'Brftop',
                # bInfo = FALSE,
                buttons = list(
                  'copy', 
                  list(
                    extend = "collection",
                    buttons = list(
                      list(extend = 'csv', filename = "regression"),
                      list(extend = 'excel', filename = "regression")
                    ),
                    text = "Download"
                  )
                )
              )
          ) %>% 
          formatPercentage(
            "Percentage",
            interval = 3,
            mark = " ",
            digits = 0
          ) %>% 
          formatStyle(
            columns = 0:9,
            valueColumns = "color_primary",
            backgroundColor = 
              styleEqual(
                sort(unique(regressionData$color_primary)), 
                sort(unique(regressionData$color_primary))
              )
          ) %>% 
          formatStyle(
            columns = 0:9,
            valueColumns = "color_secondary",
            color = 
              styleEqual(
                sort(unique(regressionData$color_secondary)), 
                sort(unique(regressionData$color_secondary))
              )
          ) 
      })
    }
  )
}