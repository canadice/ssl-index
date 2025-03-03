box::use(
  dplyr,
  shiny[tagList, div, img, span],
  reactable[reactable, colDef],
  stringr[str_detect, str_split, str_to_upper, str_trim],
  tippy[tippy],
  stats[setNames],
  purrr[pmap],
)

box::use(
  app/logic/constant,
)

#' @export
clubLogos <- function(value, index){
    Club <- currentData |> 
      dplyr$select(club) |> 
      dplyr$slice(index) |> 
      c()
    
    if(Club |> str_detect(",")){
      clubs <- str_split(Club, pattern = ",", simplify = TRUE) |> 
        c() |> 
        str_trim() |> 
        rev()
      
      list <- 
        tagList(
          lapply(
            clubs,
            function(X){
              div(
                style = "display: inline-block; width: 25px;", 
                img(src = sprintf("static/logo/%s.png", X), style = "height: 25px;", alt = X, title = X) 
              )
            }
          )
        )
      
    } else {
      # file.exists(sprintf("%s.png", Club)) |> print()
      
      image <- img(src = sprintf("static/logo/%s.png", Club), style = "height: 25px;", alt = Club, title = Club)  
      
      list <- 
        tagList(
          div(style = "display: inline-block; width: 25px;", image)
        )
    }
    
    tagList(
      div(
        class = "tableClubName",
        span(value),
        div(list)
      )
    )
}


#' @export
recordReactable <- function(currentData){
  statisticsTooltips <- 
    constant$statisticsLegend[constant$statisticsLegend$statistic %in% colnames(currentData),]
  
  currentData |>
    dplyr$mutate(
      dplyr$across(
        dplyr$where(is.numeric),
        ~ round(.x, 2)
      )
    ) |> 
    reactable(
      pagination = FALSE,
      searchable = FALSE,
      sortable = FALSE,
      defaultColDef = colDef(maxWidth = 150),
      columns =
        list(
          name = colDef(
            name = "PLAYER",
            maxWidth = 1000,
            cell = 
              function(value, index){
                Club <- currentData |> 
                  dplyr$select(club) |> 
                  dplyr$slice(index) |> 
                  c()
                
                if(Club |> str_detect(",")){
                  clubs <- str_split(Club, pattern = ",", simplify = TRUE) |> 
                    c() |> 
                    str_trim() |> 
                    rev()
                  
                  list <- 
                    tagList(
                      lapply(
                        clubs,
                        function(X){
                          div(
                            style = "display: inline-block; width: 25px;", 
                            img(src = sprintf("static/logo/%s.png", X), style = "height: 25px;", alt = X, title = X) 
                          )
                        }
                      )
                    )
                  
                } else {
                  # file.exists(sprintf("%s.png", Club)) |> print()
                  
                  image <- img(src = sprintf("static/logo/%s.png", Club), style = "height: 25px;", alt = Club, title = Club)  
                  
                  list <- 
                    tagList(
                      div(style = "display: inline-block; width: 25px;", image)
                    )
                }
                
                tagList(
                  div(
                    class = "tableClubName",
                    span(value),
                    div(list)
                  )
                )
              }
          ),
          club = colDef(show = FALSE,searchable = TRUE),
          RANK = colDef(width = 60)
        ) |> 
        append(
          pmap(statisticsTooltips, ~ {
            if((..1) %in% names(currentData)) {
              ..1 =
                colDef(
                  header =
                    tippy(..3 |> str_to_upper(), ..2, placement = "top", theme = "ssl"),
                  html = TRUE,
                  minWidth = 50
                )
            }
          }) |>
            setNames(statisticsTooltips$statistic) |>
            Filter(f = Negate(is.null), x = _)
        ) 
    )
}

#' @export
indexReactable <- function(currentData){
  statisticsTooltips <- 
    constant$statisticsLegend[constant$statisticsLegend$statistic %in% colnames(currentData),]
  
  currentData |>
    dplyr$mutate(
      dplyr$across(
        dplyr$where(is.numeric),
        ~ round(.x, 2)
      )
    ) |> 
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
            cell = clubLogos(value, index)
          ),
          club = 
            colDef(
              show = FALSE,
              searchable = TRUE
            )
        ) |> 
        append(
          pmap(statisticsTooltips, ~ {
            if((..1) %in% names(currentData)) {
              ..1 =
                colDef(
                  header =
                    tippy(..1 |> str_to_upper(), ..2, placement = "top", theme = "material"),
                  html = TRUE
                )
            }
          }) |>
            setNames(statisticsTooltips$statistic) |>
            Filter(f = Negate(is.null), x = _)
        ) 
    )
}
