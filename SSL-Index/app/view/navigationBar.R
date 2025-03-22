box::use(
  future,
  methods[is],
  shiny[moduleServer, NS, tagList, tags, icon, div, uiOutput, renderUI, observe, bindEvent, a, actionButton, p, showModal, removeModal, modalDialog, modalButton, textInput, passwordInput, verbatimTextOutput, renderText, reactive],
  shiny.router[route_link, change_page],
  shinyFeedback[feedbackWarning]
)

box::use(
  app/logic/ui/tags[flexCol, flexRow, navMenu, navMenuItem],
  app/logic/db/login[customCheckCredentials, getRefreshToken, setRefreshToken],
  app/logic/ui/spinner[withSpinnerCustom],
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
          margin-right: 12px;
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
              uiOutput(ns("jobsNavigation")) |> 
                withSpinnerCustom(height = 20),
              navMenu(
                div(a("Intro", href = route_link("/")))
              )
            )
          ),
          flexRow(
            tagList(
              verbatimTextOutput(ns("workers")),
              uiOutput(ns("yourPlayer")) |> 
                withSpinnerCustom(height = 20)  
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
    
    # workers <- reactive({
    #   print(future$nbrOfFreeWorkers())
    #   
    #   future$resetWorkers(future$plan())
    #   
    #   future$nbrOfFreeWorkers()
    #   # is(future$plan(), "multisession")
    # }) |> 
    #   bindEvent(session$clientData$url_hash)
    # 
    # output$workers <- renderText({
    #   workers()
    # })
    
    
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
      if(auth()$usergroup |> is.null()){
        navMenu(
          tagList(
            icon("user"),
            a("Login", href = "#", inputId = session$ns("login"))
          )
        )
      } else {
        flexRow(
          tagList(
            navMenu(
              tagList(
                icon("futbol"),
                a("My Player", href = route_link("myPlayer"))
              )
            ),
            navMenu(
              tagList(
                icon("door-open"),
                a("Logout", href = "#", inputId = session$ns("logout"))
              )
            )
          )
        )
      }
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
      print("login")
      
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
  })
}
