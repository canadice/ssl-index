#### REUSABLE REACTABLE FUNCTION ####
indexReactable <- function(currentData){
  statisticsTooltips <- statisticsLegend[statisticsLegend$statistic %in% colnames(currentData),]
  
  currentData %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, 2)
      )
    ) %>% 
    reactable(
      pagination = TRUE,
      searchable = TRUE,
      defaultColDef = colDef(minWidth = 100, maxWidth = 250),
      columns =
        list(
          name = colDef(
            name = "PLAYER",
            minWidth = 250,
            class = "stickyReactableColumn",
            headerClass = "stickyReactableHeader",
            cell = 
              function(value, index){
                Club <- currentData %>% 
                  .$club %>% 
                  .[index]
                
                if(Club %>% str_detect(",")){
                  clubs <- str_split(Club, pattern = ",", simplify = TRUE) %>% c() %>% rev()
                  
                  list <- 
                    tagList(
                      lapply(
                        clubs,
                        function(X){
                          div(
                            style = "display: inline-block; width: 25px;", 
                            img(src = sprintf("%s.png", X), style = "height: 25px;", alt = X) 
                          )
                        }
                      )
                    )
                  
                } else {
                  # file.exists(sprintf("%s.png", Club)) %>% print()
                  
                  image <- img(src = sprintf("%s.png", Club), style = "height: 25px;", alt = Club)  
                  
                  list <- 
                    tagList(
                      div(style = "display: inline-block; width: 25px;", image)
                    )
                }
                
                tagList(
                  div(
                    class = "tableClubName",
                    list,
                    span(value)
                  )
                )
              }
          ),
          club = 
            colDef(
              show = FALSE,
              searchable = TRUE
            )
        ) %>% 
        append(
          pmap(statisticsTooltips, ~ {
            if((..1) %in% names(currentData)) {
              ..1 =
                colDef(
                  header =
                    tippy(..1 %>% str_to_upper(), ..2, placement = "top", theme = "material"),
                  html = TRUE
                )
            }
          }) %>%
            setNames(statisticsTooltips$statistic) %>%
            Filter(Negate(is.null), .)
        ) 
    )
}

leaderReactable <- function(currentData){
  statisticsTooltips <- statisticsLegend[statisticsLegend$statistic %in% colnames(currentData),]
  
  currentData %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, 2)
      )
    ) %>% 
    reactable(
      pagination = FALSE,
      searchable = FALSE,
      defaultColDef = colDef(minWidth = 100),
      columns =
        list(
          name = colDef(
            name = "PLAYER",
            minWidth = 250,
            cell = 
              function(value, index){
                Club <- currentData %>% 
                  .$club %>% 
                  .[index]
                
                if(Club %>% str_detect(",")){
                  clubs <- str_split(Club, pattern = ",", simplify = TRUE) %>% c() %>% rev()
                  
                  list <- 
                    tagList(
                      lapply(
                        clubs,
                        function(X){
                          div(
                            style = "display: inline-block; width: 25px;", 
                            img(src = sprintf("%s.png", X), style = "height: 25px;", alt = X) 
                          )
                        }
                      )
                    )
                  
                } else {
                  # file.exists(sprintf("%s.png", Club)) %>% print()
                  
                  image <- img(src = sprintf("%s.png", Club), style = "height: 25px;", alt = Club)  
                  
                  list <- 
                    tagList(
                      div(style = "display: inline-block; width: 25px;", image)
                    )
                }
                
                tagList(
                  div(
                    class = "tableClubName",
                    list,
                    span(value)
                  )
                )
              }
          ),
          club = 
            colDef(
              show = FALSE,
              searchable = TRUE
            )
        ) %>% 
        append(
          pmap(statisticsTooltips, ~ {
            if((..1) %in% names(currentData)) {
              ..1 =
                colDef(
                  header =
                    tippy(..1 %>% str_to_upper(), ..2, placement = "top", theme = "material"),
                  html = TRUE
                )
            }
          }) %>%
            setNames(statisticsTooltips$statistic) %>%
            Filter(Negate(is.null), .)
        ) 
    )
}