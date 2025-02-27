box::use(
  shiny[NS, tags, bootstrapPage, includeCSS, moduleServer, a, icon, div, tagList, uiOutput],
  bslib,
  sass,
  shiny.router[router_ui, router_server, route, route_link],
  shinyFeedback[useShinyFeedback],
  shinyjs[useShinyjs],
)

box::use(
  app/view/welcome,
  app/logic/ui/tags[flexCol]
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    includeCSS('style.css'),
    useShinyFeedback(), # include shinyFeedback
    useShinyjs(), # include shinyjs
    tags$nav(
      class = "navbar",
      tags$ul(
        class = "nav navbar-nav",
        tags$li(
          a("Welcome", href = route_link("welcome"))
        ),
        tags$li(
          a("Chart", href = route_link("chart"))
        )
      )
    ),
    # User menu FAB
    # Wasn't able to fully customize the Rshiny FAB, so I created a new one
    tags$div(
      class = "homemade-user-fab",
      icon(
        "user",
        class = "fa-solid",
        style = "color: black; font-size: 28px; padding: 8px;",
        # Support showing and hiding user options on mobile
        ontouchstart = "
            var buttons = document.querySelector('.fab-action-buttons');
            var isShowing = getComputedStyle(buttons).opacity === '1';
            buttons.style.opacity = isShowing ? 0 : 1;
            buttons.style.visibility = isShowing ? 'hidden' : 'visible';
          "
      ),
      role = "button",
      div(
        class = "fab-action-buttons",
        tagList(
          flexCol(
            style = "gap: 4px;",
            tagList(
              uiOutput("fabOutput")  
            )
          )
        )
      )
    ),
    title = "SSL Portal",
    # sidebar = "SIDEBAR",
    # theme = theme,
    router_ui(
      route("welcome", welcome$ui(ns("message")))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    router_server("welcome")
    
    welcome$server("message", usergroup = 1)
  })
}
