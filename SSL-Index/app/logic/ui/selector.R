box::use(
  shiny,
)

box::use(
  
)


#' @export
leagueSelectInput <- function(season, session){
  if(season != "ALL"){
    season <- season |> as.numeric()
    
    if(season < 5){
      shiny$selectInput(
        inputId = session$ns("selectedLeague"),
        label = "League",
        choices = 
          c(
            "League" = "1",
            "Cup",
            "ALL"
          )
      )
    } else if (season == 12){
      shiny$selectInput(
        inputId = session$ns("selectedLeague"),
        label = "League",
        choices = 
          c(
            "Major" = "1",
            "Minor" = "2",
            "Cup",
            "WSFC",
            "ALL"
          )
      )
    } else if (season < 12){
      shiny$selectInput(
        inputId = session$ns("selectedLeague"),
        label = "League",
        choices = 
          c(
            "Division 1" = "1",
            "Division 2" = "2",
            "Cup",
            "ALL"
          )
      )
    } else {
      shiny$selectInput(
        inputId = session$ns("selectedLeague"),
        label = "League",
        choices = 
          c(
            "Major" = "1",
            "Minor" = "2",
            "Cup",
            "ALL"
          )
      )
    }
  } else {
    shiny$selectInput(
      inputId = session$ns("selectedLeague"),
      label = "League",
      choices = 
        c(
          "Major / Division 1" = "1",
          "Minor / Division 2" = "2",
          "Cup",
          "ALL"
        )
    )
  }
}
