box::use(
  shiny[div, icon, span, tagList]
)


#' @export
flexCol <- function(cont, style = "") {
  shiny::tag("div", varArgs = list(cont, class = "flex-col-wrapper", style = style))
}

#' @export
flexRow <- function(cont, style = "") {
  shiny::tag("div", varArgs = list(cont, class = "flex-row-wrapper", style = style))
}

#' @export
navMenu <- function(cont, label = "", items = list()) {
  if (!missing(cont) && label == "") {
    shiny::tag("div", varArgs = list(cont, class = "nav-toggle"))
  } else if (length(label) > 0) {
    div(
      class = "nav-toggle",
      tagList(
        flexRow(
          style = "align-items: center; gap: 4px;",
          tagList(
            span(label),
            icon("caret-down")
          )
        ),
        if (length(items) > 0) {
          div(
            class = "nav-toggle_items",
            flexCol(
              tagList(
                tags_list <- lapply(items, function(item) {
                  div(
                    class = "nav-toggle_item",
                    item
                  )
                })
              )
            )
          )
        }
      )
    )
  }
}

#' @export
navMenuItem <- function(cont, label = "", subItems = list()) {
  if (length(subItems) > 0) {
    tagList(
      flexRow(
        style = "align-items: center; justify-content: space-between; gap: 4px; padding: 8px;",
        tagList(
          span(label),
          icon("caret-right")
        )
      ),
      div(
        class = "nav-toggle_sub-items",
        div(
          tagList(
            tags_list <- lapply(subItems, function(item) {
              div(
                class = "nav-toggle_sub-item",
                item
              )
            })
          )
        )
      )
    )
  } else {
    cont
  }
}