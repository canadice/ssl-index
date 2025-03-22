box::use(
  bslib,
  dplyr[select, if_else, mutate, arrange, slice_head, desc],
  shiny,
  stringr[str_replace_all],
)


#' @export
resultCard <- function(data, i){
  bslib$card(
    bslib$card_header(
      shiny$div(
        shiny$div(style = "display: inline-block; width: 40px;", shiny$img(src = sprintf("static/logo/%s (Custom).png", data[i, "Home"]), style = "height: 40px;", alt = data[i, "Home"], title = data[i, "Home"])), 
        shiny$strong(" - "), 
        shiny$div(style = "display: inline-block; width: 40px;", shiny$img(src = sprintf("static/logo/%s (Custom).png", data[i, "Away"]), style = "height: 40px;", alt = data[i, "Away"], title = data[i, "Away"])),
        align = "center"
      )
    ),
    bslib$card_body(
      shiny$h4(paste(data[i, "HomeScore"], data[i, "AwayScore"], sep = "-") |> 
                 str_replace_all(pattern = "NA", replacement = " ")
      )
    ),
    bslib$card_footer(
      paste(
        paste(
          if_else(data[i, "MatchType"] == 0, 
                  "Cup",
                  if_else(data[i, "MatchType"] == 1, 
                          "Major League",
                          if_else(data[i, "MatchType"] == 2, "Minor League", 
                                  if_else(data[i, "MatchType"] == 5, "WSFC","Friendlies")))),
          data[i, "MatchDay"], sep = ", "
        ),
        paste(
          data[i, "IRLDate"]
        ),
        sep = "<br>"
      ) |> 
        shiny$HTML() |> 
        shiny$div(align = "center")
    )
  )
}
