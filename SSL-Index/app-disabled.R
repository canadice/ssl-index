#### SSL PORTAL DOWN FOR MAINTENANCE ####
suppressMessages({
  ## Data handling
  require(tidyverse, quietly = FALSE)
  require(bslib, quietly = FALSE)
})

ui <- page_fluid(
  h1("SSL PORTAL CURRENTLY UNDER MAINTENANCE")
)

server <- function(input, output){
  
}

# Run the application 
app <- shinyApp(ui = ui, server = server, enableBookmarking = "url")
