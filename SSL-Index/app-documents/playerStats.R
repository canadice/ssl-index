
############################################################################
############################################################################
###                                                                      ###
###                 POSITION TRACKER CREATED FOR THE SHL                 ###
###                                                                      ###
############################################################################
############################################################################


### UI module for player similarities using MDS
playerStatsUI <- function(id){
  
  ## Creating the namespacing function for all IDs
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 10,
          offset = 1,
          DTOutput(outputId = ns("playerStats"))
        )
      )
    )
 )
}

## Backend module for player similarities
playerStatsSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){

      ###  Reading the data
           
      url <- "https://raw.githack.com/canadice/ssl-index/main/SSL-Index/data/playerStats.html"
      
      ###  Processing the data                                         
      
      playerStats <- 
        url %>% 
        read_html() %>% 
        html_elements("table") %>% 
        html_table() %>% 
        .[[1]] %>% 
        rename(
          Information = `Inf`,
          Appearances = Apps,
          `Minutes per Game` = `Mins/Gm`,
          Goals = Gls,
          Assists = Ast,
          `Completed Passes` = `Ps C`,
          `Pass%` = `Pas %`,
          `Shots on Target` = ShT,
          `Tackles Won` = `Tck W`,
          `Tackles Lost` = `Tck A`,
          Dribbles = `Drb`,
          `Completed Crosses` = `Cr C`,
          `Completed Attempted` = `Cr A`,
          Headers = Hdrs,
          `Headers%` = `Hdr %`,
          Interceptions = Itc,
          Fouls = Fls,
          `Distance Run (mi)` = Distance,
          `Average Rating` = `Av Rat`,
          `Player of the Match` = `PoM`
        ) %>% 
        mutate(
          # Position = NA,
          Nationality = 
            Name %>% 
            str_split(
              pattern = " - ", 
              simplify = TRUE
            ) %>% 
            .[,2],
          Name = 
            Name %>% 
            str_split(
              pattern = " - ", 
              simplify = TRUE
            ) %>% 
            .[,1],
          `Pass%` = 
            `Pass%` %>% 
            str_remove_all(
              pattern = "%"
            ) %>% 
            as.numeric()/100,
          `Headers%` =
            `Headers%` %>% 
            str_remove_all(
              pattern = "%"
            ) %>% 
            as.numeric()/100,
          `Distance Run (mi)` = 
            `Distance Run (mi)` %>% 
            str_remove_all(
              pattern = "mi"
            ) %>% 
            as.numeric()
        ) %>% 
        mutate(
          across(
            !contains(
              c("Name", "Information", "Nationality", "Position")
            ),
            as.numeric
          )
        ) %>% 
        relocate(
          c(
            Nationality,
            Position,
            Appearances,
            `Minutes per Game`,
            `Distance Run (mi)`
          ),
          .after = Name
        ) %>% 
        select(
          -Information
        ) %>% 
        arrange(
          `Average Rating`
        ) %>% 
        # As there are non-numeric values being transformed correctly to NA, warnings are suppressed. 
        suppressWarnings()
        
      
      output$playerStats <- renderDT({
        if(length(playerStats) == 0){
          NULL
        } else {
          playerStats  
        }
      },
      class = 'compact cell-border stripe',
      rownames = FALSE,
      escape = FALSE,
      options = 
        list(
          ordering = TRUE, 
          ## Sets a scroller for the rows
          scrollY = '80%',
          sScrollX = "100%",
          ## Sets size of rows shown
          scrollCollapse = TRUE,
          pageLength = 10,
          dom = 'Rftp',
          ## Sets color of table background
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}")
        )
      )
    }
  )
}

