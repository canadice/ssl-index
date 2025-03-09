box::use(
  shiny[moduleServer, NS, tagList, tags, icon, div, uiOutput, renderUI, observe, bindEvent, a, actionButton, p, showModal, removeModal, modalDialog, modalButton, textInput, passwordInput],
  shiny.router[route_link, change_page],
  shinyFeedback[feedbackWarning]
)

box::use(
  app/logic/ui/tags[flexCol, flexRow, navMenu, navMenuItem],
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
      class = "ssl-navbar",
      flexRow(
        style = "
          margin-left: 128px;
          margin-right: 24px;
          height: inherit;
          align-items: end;
          justify-content: space-between;
        ",
        tagList(
          flexRow(
            tagList(
              tags$a(
                href='https://forum.simulationsoccer.com',
                target="_blank",
                tags$img(src = 'static/portalwhite.png', height = "70"),
                class = "logo"
              ),
              navMenu(
                label = "Trackers",
                items = list(
                  a("Players", href = route_link("tracker/player")),
                  a("Organizations", href = route_link("tracker/organization")),
                  a("Draft Class", href = route_link("tracker/draftclass"))
                )
              ),
              navMenu(
                label = "Index",
                items = list(
                  a("Index", href = route_link("index/")),
                  a("Records", href = route_link("index/records")),
                  a("Standings", href = route_link("index/standings")),
                  a("Schedule", href = route_link("index/schedule")),
                  a("Academy", href = route_link("index/academy"))
                )
              ),
              uiOutput(ns("jobsNavigation")),
              navMenu(
                div(a("Intro", href = route_link("/")))
              )
            )
          ),
          flexRow(
            uiOutput(ns("yourPlayer"))
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
      if (any(c(3, 4, 8, 11, 12, 14, 15) %in% auth()$usergroup)) {
        items <- list(
          if (any(c(4, 3, 14) %in% auth()$usergroup)) {
            navMenuItem(
              label = "File Work",
              subItems = list(
                a("Build Exports", href = route_link("filework/export")),
                a("Index Imports", href = route_link("filework/import")),
                a("Edit Schedule", href = route_link("filework/schedule"))
              )
            )
          }
        )
        navMenu(
          label = "Jobs",
          items = items
        )
      }
    }) |> 
      bindEvent(auth())
    
    output$yourPlayer <- renderUI({
      navMenu(
        a("My Player", href = route_link("myPlayer"))
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
