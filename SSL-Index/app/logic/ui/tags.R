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
    shiny::tag("div", varArgs = list(cont, class = "nav-menu"))
  } else if (length(label) > 0) {
    div(
      class = "nav-menu",
      role = "button",
      onclick = "
        const allMenuItems = document.querySelectorAll('.nav-menu_items');
        const childMenuItems = this.querySelector('.nav-menu_items');
        if (childMenuItems) {
          const isClosed = getComputedStyle(childMenuItems).height === '0px';

          // Close all other open menus
          if (isClosed) {
            allMenuItems.forEach(item => {
              if (item !== childMenuItems) {
                item.style.height = '0px';
              }
            });
          }

          // Show child menu items if closed, otherwise hide them
          childMenuItems.style.height = isClosed ? 'max-content' : '0px';
        }
      ",
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
            class = "nav-menu_items",
            role = "button",
            flexCol(
              tagList(
                tags_list <- lapply(items, function(item) {
                  div(
                    class = "nav-menu_item",
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
        style = "align-items: center; justify-content: space-between; gap: 4px;",
        tagList(
          span(label, role = "button"),
          div(
            class = "nav-menu_item-caret-right",
            icon("caret-right")
          ),
          div(
            class = "nav-menu_item-caret-down",
            icon("caret-down")
          )
        )
      ),
      div(
        class = "nav-menu_sub-items",
        role = "button",
        div(
          tagList(
            tags_list <- lapply(subItems, function(item) {
              div(
                class = "nav-menu_sub-item",
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