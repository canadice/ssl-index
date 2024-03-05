
attributeEdit <- function(name, value, session){
  if(stringr::str_detect(name, pattern = "Fitness|Stamina")){
    splitLayout(
      name %>% 
        paste("*", sep = "") %>% 
      span(
        class = "hovertext", 
        data_hover = attributes$explanation[attributes$attribute == name]
      ) %>% 
        div(class = "attribute"),
      numericInput(
        inputId = session$ns(name),
        label = NULL,
        value = 20,
        min = 20,
        max = 20,
        width = NULL
      ) %>% 
        div(class = "attributeInput"),
      "&nbsp;" %>% 
        HTML() %>% 
        div(class = "attributeCost"),
      cellWidths = c("50%", "25%", "25%"),
      cellArgs = list(style = "white-space: initial; padding: 5px")
    )
  } else {
    splitLayout(
      name %>% 
        span(
          class = "hovertext", 
          data_hover = attributes$explanation[attributes$attribute == name]
        ) %>% 
        div(class = "attribute"),
      numericInput(
        inputId = session$ns(name),
        label = NULL,
        value = value,
        min = 5,
        max = 20,
        width = NULL
      ) %>% 
        div(class = "attributeInput"),
      uiOutput(session$ns(paste("cost", name, sep = ""))) %>% 
        div(class = "attributeCost"),
      cellWidths = c("50%", "25%", "25%"),
      cellArgs = list(style = "white-space: initial; padding: 5px")
    )
  }
}

