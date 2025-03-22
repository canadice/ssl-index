box::use(
  bslib,
  dplyr,
  future[nbrOfFreeWorkers],
  plotly,
  promises[future_promise, then],
  reactable[reactableOutput],
  shiny,
  shiny.router[change_page, get_query_param],
  stringr[str_detect, str_remove],
)

box::use(
  app/logic/db/api[readAPI],
  app/logic/db/get[getPlayerNames, getPlayer],
  app/logic/ui/player[playerOutput],
  app/logic/ui/spinner[withSpinnerCustom],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$tagList(
    bslib$card(
      bslib$card_header(
        bslib$layout_column_wrap(
          width = NULL,
          style = bslib$css(grid_template_columns = "1fr 2fr 3fr"),
          shiny$selectInput(
            ns("selectedPlayer"),
            "Select Player",
            choices = NULL
          ),
          shiny$tagList(
            shiny$radioButtons(
              ns("retired"),
              label = "Include retired: ",
              choices = c("Yes" = TRUE, "No" = FALSE),
              inline = TRUE
            ),
            shiny$radioButtons(
              ns("freeAgent"),
              label = "Include free agents: ",
              choices = c("Yes" = TRUE, "No" = FALSE),
              inline = TRUE
            )
          ),
          ""
        )
      ),
      bslib$card_body(
        shiny$tagList(
          bslib$layout_column_wrap(
            width = 1 / 2,
            bslib$card(
              bslib$card_header(
                shiny$h3("Profile Information")
              ),
              bslib$card_body(
                bslib$layout_column_wrap(
                  width = NULL,
                  style = bslib$css(grid_template_columns = "3fr 1fr"),
                  shiny$uiOutput(ns("playerName")) |> 
                    withSpinnerCustom(height = 20),
                  shiny$uiOutput(ns("clubLogo"), height = NULL) |>
                    withSpinnerCustom(height = 20)
                ),
                shiny$uiOutput(ns("playerInfo")) |>
                  withSpinnerCustom(height = 40)
              )
            ),
            bslib$card(
              bslib$card_header(
                shiny$h3("Recent Match Statistics")
              ),
              bslib$card_body(
                reactableOutput(ns("matchStatistics")) |>
                  withSpinnerCustom(height = 60)
              )
            )
          ),
          bslib$layout_column_wrap(
            width = NULL,
            style = bslib$css(grid_template_columns = "2fr 1fr"),
            bslib$card(
              bslib$card_header(
                shiny$h3("Player Attributes")
              ),
              bslib$card_body(
                shiny$uiOutput(ns("playerAttributes")) |>
                  withSpinnerCustom(height = 60)
              )
            ),
            bslib$card(
              bslib$card_header(
                shiny$h3("TPE Progression")
              ),
              bslib$card_body(
                plotly$plotlyOutput(ns("tpeProgression")) |>
                  withSpinnerCustom(height = 60)
              )
            )
          ),
          bslib$card(
            bslib$card_header(
              shiny$h3("Player History")
            ),
            bslib$card_body(
              shiny$uiOutput(ns("playerHistory")) |>
                withSpinnerCustom(height = 60)
            )
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    #### Data ####
    allPlayers <- shiny$reactive({
      retired <- input$retired |> as.logical()
      fa <- input$freeAgent |> as.logical()
      
      future_promise({
        print(Sys.getpid())
        
        getPlayerNames(retired = retired, freeAgent = fa)
      })
    })

    playerData <- shiny$reactive({
      shiny$req(input$selectedPlayer)

      pid <- input$selectedPlayer |>
        as.numeric()

      
      future_promise({
        print(Sys.getpid())
        
        getPlayer(pid)
      })
    }) |>
      shiny$bindEvent(input$selectedPlayer)

    query <- shiny$reactive({
      shiny$req(allPlayers())
      pid <- get_query_param("pid")

      if (is.null(pid)) {
        NULL
      } else {
        pid |>
          as.numeric()
      }
    })

    #### Output ####

    #### Observe ####
    shiny$observe({
      allPlayers() |>
        then(
          onFulfilled = function(names) {
            namedVector <- names$pid

            names(namedVector) <- names$name

            shiny$updateSelectInput(
              session = session,
              inputId = "selectedPlayer",
              choices = namedVector
            )

            change_page(paste0("tracker/player?pid=", names$pid[1]))
          }
        )
    }) |>
      shiny$bindEvent(allPlayers())

    shiny$observe({
      playerData() |>
        then(
          onFulfilled = function(data) {
            print("Starting rendering output")
            playerOutput(data, input, output, session)
            print("Finishing rendering output")
          }
        )
    }) |>
      shiny$bindEvent(playerData())


    shiny$observe({
      current <- str_remove(session$clientData$url_hash,
                            pattern = "#!/")
      
      if ((query() |> is.null()) | !(current |> str_detect("tracker/player"))) {
        # change_page(paste0("tracker/player?pid=", input$selectedPlayer))
      } else {
        if (input$selectedPlayer != query()) {
          change_page(paste0("tracker/player?pid=", input$selectedPlayer))
        }
      }
    }) |>
      shiny$bindEvent(input$selectedPlayer)
  })
}
