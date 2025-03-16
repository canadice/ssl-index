box::use(
  bslib,
  dplyr,
  promises[future_promise, then],
  shiny,
  shiny.router[change_page, get_query_param],
)

box::use(
  app/logic/db/api[readAPI],
  app/logic/ui/player[playerOutput, playerOutputUI],
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
              choices = c("Yes" = 1, "No" = 0),
              inline = TRUE
            ),
            shiny$radioButtons(
              ns("freeAgent"),
              label = "Include free agents: ",
              choices = c("Yes" = 1, "No" = 0),
              inline = TRUE
            )
          ),
          ""
        )
      ),
      bslib$card_body(
        shiny$uiOutput(ns("playerOutput")) |>
          withSpinnerCustom(height = 200)
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    #### Data ####
    allPlayers <- shiny$reactive({
      readAPI(url = "https://api.simulationsoccer.com/player/getAllPlayers") |>
        dplyr$select(name, pid, username, team, status_p) |>
        future_promise()
    })

    playerData <- shiny$reactive({
      shiny$req(input$selectedPlayer)

      pid <- input$selectedPlayer |>
        as.numeric()

      readAPI(
        url = "https://api.simulationsoccer.com/player/getPlayer",
        query = list(pid = pid)
      ) |>
        future_promise()
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
    output$playerOutput <- shiny$renderUI({
      playerOutputUI(session)
    })


    #### Observe ####
    shiny$observe({
      allPlayers() |>
        then(
          onFulfilled = function(names) {
            names <-
              names |>
              dplyr$filter(if (input$retired != 1) status_p > 0 else TRUE) |>
              dplyr$filter(if (input$freeAgent != 1) !(team %in% c("FA", "Retired")) else TRUE) |>
              dplyr$arrange(name)

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
      shiny$bindEvent(allPlayers(), once = TRUE)

    shiny$observe({
      playerData() |>
        then(
          onFulfilled = function(data) {
            playerOutput(data, input, output, session)
          }
        )
    }) |>
      shiny$bindEvent(playerData())


    shiny$observe({
      if ((query() |> is.null())) {
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
