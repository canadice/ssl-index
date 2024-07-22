
############################################################################
############################################################################
###                                                                      ###
###                  APPLICATION FOR THE WELCOME SCREEN                  ###
###                                                                      ###
############################################################################
############################################################################

welcomeUI <- function(id){
  ## Creating the namespacing function for all IDs
  ns <- NS(id)
  
  tagList(
    ## Welcome text
    withMathJax(
      includeMarkdown(
        "app-documents/mdWelcome.md"
      )
    )
  )
}