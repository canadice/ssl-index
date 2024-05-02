createPlayerUI <- function(id) {
  ns <- NS(id)
  tagList(
    "This page is blank"
  )
}

createPlayerServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}