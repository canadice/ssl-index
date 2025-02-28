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
            "ALL",
            "League" = "1",
            "Cup"
          )
      )
    } else if (season == 12){
      shiny$selectInput(
        inputId = session$ns("selectedLeague"),
        label = "League",
        choices = 
          c(
            "ALL",
            "Major" = "1",
            "Minor" = "2",
            "Cup",
            "WSFC"
          )
      )
    } else if (season < 12){
      shiny$selectInput(
        inputId = session$ns("selectedLeague"),
        label = "League",
        choices = 
          c(
            "ALL",
            "Division 1" = "1",
            "Division 2" = "2",
            "Cup"
          )
      )
    } else {
      shiny$selectInput(
        inputId = session$ns("selectedLeague"),
        label = "League",
        choices = 
          c(
            "ALL",
            "Major" = "1",
            "Minor" = "2",
            "Cup"
          )
      )
    }
  } else {
    shiny$selectInput(
      inputId = session$ns("selectedLeague"),
      label = "League",
      choices = 
        c(
          "ALL",
          "Major / Division 1" = "1",
          "Minor / Division 2" = "2",
          "Cup"
        )
    )
  }
}
