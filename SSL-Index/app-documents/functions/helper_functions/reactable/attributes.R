attributeReactable <- function(data, session, output){
  visData <- 
    data |> 
    select(acceleration:throwing) |> 
    select(where(~ !is.na(.x))) |> 
    pivot_longer(
      cols = everything(),
      values_to = "Value",
      names_to = "Attribute"
    ) |> 
    mutate(
      Attribute = str_to_title(Attribute)
    ) |> 
    left_join(
      attributes,
      by = c("Attribute" = "attribute") 
    ) |> 
    mutate(
      Attribute = factor(Attribute, levels = sort(Attribute |> unique(), decreasing = TRUE)),
      group = factor(group, levels = c("Physical", "Mental", "Technical", "Goalkeeper")),
      ValueFill = case_when(
        Value >= 18 ~ 1,
        Value >= 13 ~ 2,
        Value >= 10 ~ 3,
        TRUE ~ 5
      ) |> factor()
    ) |> 
    {
      if(data$pos_gk == 20){
        filter(
          ., 
          (group %in% c("Goalkeeper", "Technical") & keeper == "TRUE") | (group %in% c("Physical", "Mental"))
        )
      } else {
        filter(
          .,
          group %in% c("Physical", "Mental", "Technical")
        )
      }
    }
  
  map(.x = visData$group |> unique() |> sort(),
      .f = function(chosenGroup){
        output[[chosenGroup]] <- renderReactable({
          temp <- 
            visData |> 
            filter(
              group == chosenGroup
            )
          
          temp |> 
            select(Attribute, Value) |> 
            reactable(
              defaultColDef = colDef(
                style = function(value, index){
                  color <- if_else(temp$ValueFill[index] == 1, "#66b38c", 
                                   if_else(temp$ValueFill[index] == 2, "#F5D17E", 
                                           if_else(temp$ValueFill[index] == 3, "#ffffff", "#B6B6B6")
                                           )
                                   )
                  # color <- if_else(temp$ValueFill[index] == 1, "#008450", if_else(temp$ValueFill[index] == 2, "#EFB700", "#B81D13")) 
                  list(background = color, color = "black")
                }
              ),
              columns = list(
                Value = colDef(name = "", width = 40)
              ),
              pagination = FALSE,
              sortable = FALSE
            )
        })
      })
  
  map(.x = visData$group |> unique() |> sort(),
      .f = function(group){
        column(width = 12 / length(visData$group |> unique()),
               h4(group),
               reactableOutput(session$ns(group))
        )
      }) |> 
    tagList()
}
