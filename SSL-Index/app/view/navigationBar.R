box::use(
  shiny[moduleServer, NS, tagList, tags, icon, div, uiOutput, renderUI, observe, bindEvent, a, actionButton, p, showModal, removeModal, modalDialog, modalButton, textInput, passwordInput],
  shiny.router[route_link, change_page],
  shinyFeedback[feedbackWarning]
)

box::use(
  app/logic/ui/tags[flexCol, flexRow],
  app/logic/db/login[customCheckCredentials, getRefreshToken, setRefreshToken],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    ## Function that loads js-cookies for auto-login
    tags$script(
      src = paste0(
        "https://cdn.jsdelivr.net/npm/js-cookie@rc/",
        "dist/js.cookie.min.js"
      )
    ),
    ## js function for storing cookies
    tags$script(
      src = paste0(
        "https://cdn.jsdelivr.net/npm/js-cookie@rc/",
        "dist/js.cookie.min.js"
      )
    ),
    tags$script("// script.js
                    function getCookies(){
                      var res = Cookies.get();
                      Shiny.setInputValue('cookies', res);
                    }
                  
                  // script.js
                    Shiny.addCustomMessageHandler('cookie-set', function(msg){
                      Cookies.set(msg.name, msg.value);
                      getCookies();
                    })
                    
                    Shiny.addCustomMessageHandler('cookie-remove', function(msg){
                      Cookies.remove(msg.name);
                      getCookies();
                    })
                  
                  $(document).on('shiny:connected', function(ev){
                    getCookies();
                  });"),
    tags$script("Shiny.addCustomMessageHandler('cookie-remove', function(msg){
                      Cookies.remove(msg.name);
                      getCookies();
                    })
                  "),
    tags$head(
      tags$link(
        rel = "icon", 
        type = "image/png", 
        href = "favicon.ico"),
      tags$title("SSL Portal")
    ),
    tags$nav(
      class = "navbar",
      flexRow(
        style = "align-items: end;",
        tagList(
          tags$a(
            href='https://forum.simulationsoccer.com',
            target="_blank",
            tags$img(src = 'static/portalblack.png', height = "70"),
            style = "margin-right: 12px;"
          ),
          uiOutput(ns("yourPlayer")),
          div(
            class = "nav-toggle",
            tagList(
              tags$span("Trackers"),
              div(
                class = "nav-toggle_items",
                flexCol(
                  tagList(
                    div(a("Players", href = route_link("tracker/player"))),
                    div(a("Organizations", href = route_link("tracker/organization"))),
                    div(a("Draft Class", href = route_link("tracker/draftclass")))
                  )
                )
              )
            )
          ),
          div(
            class = "nav-toggle",
            tagList(
              tags$span("Index"),
              div(
                class = "nav-toggle_items",
                flexCol(
                  tagList(
                    div(a("Index", href = route_link("index/"))),
                    div(a("Records", href = route_link("index/records"))),
                    div(a("Standings", href = route_link("index/standings"))),
                    div(a("Schedule", href = route_link("index/schedule"))),
                    div(a("Academy", href = route_link("index/academy")))
                  )
                )
              )
            )
          ),
          uiOutput(ns("jobsNavigation")),
          div(
            class = "nav-toggle",
            div(a("Intro", href = route_link("/")))
          ),
        )
      )
    ),
    # User menu FAB
    # Wasn't able to fully customize the Rshiny FAB, so I created a new one
    div(
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
              uiOutput(ns("fabOutput"))
            )
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, auth, resAuth) {
  moduleServer(id, function(input, output, session) {
    
    ### Output
    output$jobsNavigation <- renderUI({
      # if(any(c(3, 4, 8, 11, 12, 14, 15) %in% auth()$usergroup)){
        div(
          class = "nav-toggle",
          tagList(
            tags$span("Jobs"),
            div(
              class = "nav-toggle_items",
              flexCol(
                # class = "nav-toggle",
                tagList(
                  # if(any(c(4, 3, 14) %in% auth()$usergroup)){
                    div(a("Build Exports", href = route_link("filework/export"))),
                    div(a("Index Imports", href = route_link("filework/import"))),
                    div(a("Edit Schedule", href = route_link("filework/schedule"))),
                  # }
                )
              )
            )
          )
        )
    }) |> 
      bindEvent(auth())
    
    output$yourPlayer <- renderUI({
      div(
        class = "nav-toggle",
        a("Player", href = route_link("myPlayer"))
      )
    }) |> 
      bindEvent(auth())
    
    ### Observers
    # Checks saved cookie for automatic login
    observe({
      refreshtoken <- getRefreshToken(input$cookies$token)
      if(refreshtoken |> nrow() > 0){
        if((now() |> as.numeric()) < refreshtoken$expires_at){
          resAuth$uid <- refreshtoken$uid
          resAuth$username <- refreshtoken$username
          resAuth$usergroup <- 
            paste(refreshtoken$usergroup, refreshtoken$additionalgroups, sep = ",") |>
            str_split(pattern = ",", simplify = TRUE) |>
            as.numeric() |>
            as.list()
          
          setRefreshToken(uid = refreshtoken$uid, token = refreshtoken$token)
          
          # session$reload()
        }
      }
    }) |>
      bindEvent(input$cookies$token, ignoreNULL = TRUE, once = TRUE)
    
    observe({
      showModal(
        modalDialog(
          textInput(session$ns("user"), label = "Username:"),
          passwordInput(session$ns("password"), label = "Password:"),
          footer = tagList(
            modalButton("Cancel"), actionButton(session$ns("loggingIn"), "Login"),
            tags$div(
              tags$a("Register a new user!", href = "https://forum.simulationsoccer.com/member.php?action=register", target = "_blank", style = "float: left;"),
              tags$a("Forgot password?", href = "https://forum.simulationsoccer.com/member.php?action=lostpw", target = "_blank", style = "float:right;")  
            )
          )
        )
      )
    }) |> 
      bindEvent(input$login)
    
    observe({
      res <- customCheckCredentials(user = input$user, password = input$password)
      if(res$result){
        removeModal()
        resAuth$uid <- res$userInfo$uid
        resAuth$username <- res$userInfo$username
        resAuth$usergroup <- res$userInfo$usergroup
      } else {
        feedbackWarning("password", show = TRUE, text = "Password is incorrect.")
      }
    }) |> 
      bindEvent(input$loggingIn)
    
    output$fabOutput <- renderUI({
      tagList(
        if(auth()$usergroup |> is.null()){
          tagList(
            actionButton(
              inputId = session$ns("login"),
              label = "Login",
              icon = icon("sign-in"),
              class = "centered-flex-content",
              style = "justify-content: flex-start; gap: 8px;"
            )
          )
        } else if(any(c(0,5) %in% auth()$usergroup)) {
          p("You are a banned user.")
        } else {
          tagList(
            # User menu items. Can be extended by adding more actionButtons.
            # Just remember to connect any button's click event to an action server-side.
            actionButton(
              inputId = session$ns("player"),
              label = "Your Player",
              icon = icon("futbol"),
              class = "centered-flex-content",
              style = "justify-content: flex-start; gap: 8px;"
            ),
            actionButton(
              inputId = session$ns("userbank"),
              label = "Bank/Store",
              icon = icon("building-columns"),
              class = "centered-flex-content",
              style = "justify-content: flex-start; gap: 8px;"
            ),
            actionButton(
              inputId = session$ns("logout"),
              label = "Logout",
              icon = icon("door-open"),
              class = "centered-flex-content",
              style = "justify-content: flex-start; gap: 8px;"
            )
          )
        }
      )
    })
    
    # Observers tied to the actionButtons in the FAB
    observe({
      change_page("yourPlayer")
    }) |> 
      bindEvent(input$player)
    
    observe({
      change_page("playerStore")
    }) |> 
      bindEvent(input$userbank)
    
    observe({
      resAuth$uid <- resAuth$username <- resAuth$usergroup <- NULL
      
      change_page("/")
      # Removes cookies when logging out
      session$sendCustomMessage("cookie-remove", list(name = "token"))
    }) |> 
      bindEvent(input$logout)
  })
}
