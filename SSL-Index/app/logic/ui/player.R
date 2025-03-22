box::use(
  bslib,
  dplyr,
  lubridate[as_date, as_datetime, floor_date, today],
  plotly,
  promises[then, promise_all],
  reactable[colDef, colFormat, reactable, renderReactable],
  rlang[is_empty],
  shiny,
  stringr[str_remove, str_split, str_to_upper],
  tidyr[complete, pivot_longer],
)

box::use(
  app/logic/constant,
  app/logic/db/api[readAPI],
  app/logic/db/get[getBankHistory, getUpdateHistory, getTpeHistory],
  app/logic/ui/reactableHelper[attributeReactable, recordReactable],
  app/logic/ui/spinner[withSpinnerCustom],
  app/logic/ui/tags[flexCol, flexRow],
)


#' @export
playerOutput <- function(data, input, output, session){
  print("Now we are in the output section")
  
  output$playerName <- shiny$renderUI({
    print(paste0("Rendering player name", Sys.time()))
    
    shiny$tagList(
      shiny$h2(paste(data$name, paste0("(", data$class, ")"), sep = " ")),
      shiny$h3(paste0("@", data$username))
    )
  }) 
  
  output$clubLogo <- shiny$renderUI({
    print(paste0("Rendering club logo", Sys.time()))
    shiny$img(
      src = sprintf("static/logo/%s.png", data$team),
      style = "height: 100px;",
      alt = data$team,
      title = data$team
    )
  })
  
  output$playerInfo <- shiny$renderUI({
    print(paste0("Rendering player info", Sys.time()))
    value <-
      data |>
      dplyr$select(
        dplyr$contains("pos_")
      ) |>
      pivot_longer(
        dplyr$everything()
      ) |>
      dplyr$mutate(
        name = str_remove(name, pattern = "pos_") |>
          str_to_upper()
      )
    
    shiny$tagList(
      bslib$layout_columns(
        col_widths = c(6, 6),
        shiny$tagList(
          shiny$h4(paste("TPE: ", data$tpe)),
          shiny$h4(paste("Banked TPE: ", data$tpebank)),
          # shiny$h4(paste("Bank Balance: ", data$bank)),
          shiny$h4(paste("Player Status: "), data$playerStatus, class = data$playerStatus),
          shiny$h4(paste("User Status: "), data$userStatus, class = data$userStatus),
          shiny$h5(paste("Nationality:"), data$nationality),
          shiny$h5(paste("Render: "), data$render)
        ),
        shiny$tagList(
          shiny$h4("Traits"),
          data$traits |>
            str_split(pattern = constant$traitSep) |>
            unlist() |>
            paste(collapse = "<br>") |>
            shiny$HTML(),
          shiny$br(),
          shiny$h4("Primary Position(s)"),
          value |>
            dplyr$filter(value == 20) |>
            dplyr$select(name) |>
            unlist() |>
            paste(collapse = ", ") |>
            shiny$HTML(),
          shiny$h4("Secondary Position(s)"),
          value |>
            dplyr$filter(value < 20, value >= 10) |>
            dplyr$select(name) |>
            unlist() |>
            paste(collapse = ", ") |>
            shiny$HTML()
        )
      )
    )
  })
  
  output$matchStatistics <- renderReactable({
    if (data$pos_gk == 20){
      matches <-
        readAPI(
          url = "https://api.simulationsoccer.com/index/latestGames",
          query = list(name = data$name, outfield = FALSE)
        )
    } else {
      matches <-
        readAPI(
          url = "https://api.simulationsoccer.com/index/latestGames",
          query = list(name = data$name)
        )
    }
    
    print(paste0("Rendering player games", Sys.time()))
    
    if (!(matches |> is_empty())){
      matches |>
        recordReactable()
    } else {
      NULL
    }
  })
  
  output$playerAttributes <- shiny$renderUI({
    print(paste0("Rendering player attributes", Sys.time()))
    attributeReactable(data, session, output)
  })
  
  output$tpeProgression <- plotly$renderPlotly({
    historyTPE() |>
      then(
        onFulfilled = function(tpe){
          print(paste0("Rendering player progression", Sys.time()))
          if(nrow(tpe) < 2){
            plotly$plot_ly(mode = "markers", type = "scatter") |>
              plotly$add_annotations(
                text = "The player has had no TPE<br>progression in the Portal",
                x = 0.5, y = 0.5,
                xref = "paper", yref = "paper",
                showarrow = FALSE,
                font = list(size = 20),
                align = "center",
                borderpad = 10,
                bgcolor = "rgba(255, 255, 255, 0.5)"
              ) |>
              plotly$layout(
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showline = FALSE, showticklabels = FALSE),
                margin = list(l = 0, r = 0, b = 0, t = 0),
                plot_bgcolor = "#333333",   # background color
                paper_bgcolor = "#333333"
              ) |>
              plotly$config(
                displayModeBar = TRUE,  # Enable display of mode bar (optional, true by default)
                modeBarButtonsToRemove = list(
                  "toImage", "zoom2d", "pan2d", "select2d",
                  "lasso2d", "zoomIn2d", "zoomOut2d",
                  "autoScale2d", "resetScale2d"
                ),
                displaylogo = FALSE  # Remove Plotly logo
              )
          } else {
            visData <-
              tpe |>
              dplyr$mutate(
                WeekStart =
                  floor_date(Time |>
                               as_date(),
                             "week",
                             week_start = 1)
              ) |>
              dplyr$group_by(WeekStart) |>
              dplyr$summarize(total = sum(`TPE Change`, na.rm = TRUE)) |>
              complete(
                WeekStart =
                  seq(
                    min(WeekStart),
                    floor_date(today() |>
                                 as_date(tz = "US/Pacific"),
                               "week",
                               week_start = 1),
                    by = "week"
                  ),
                fill = list(total = 0)
              ) |>
              dplyr$ungroup() |>
              dplyr$mutate(cumulative = cumsum(total),
                           week = seq_len(dplyr$n())) |>
              suppressMessages()
            
            plotly$plot_ly(visData, hoverinfo = "text") |>
              plotly$add_trace(x = ~week, y = ~cumulative, type = "scatter", mode = "markers+lines",
                               line = list(color = constant$sslGold),
                               marker = list(size = 5, color = constant$sslGold),
                               text = ~paste("Week:", week, "<br>TPE:", cumulative)
              ) |>
              plotly$layout(
                title = list(
                  text = "TPE Progression",
                  font = list(color = "white")  # Set title text color to white
                ),
                xaxis = list(
                  title = "Time",
                  tickfont = list(color = "white"),  # Set x-axis tick labels color to white
                  titlefont = list(color = "white"),  # Set x-axis title color to white
                  dtick = 1,
                  showgrid = FALSE
                ),
                yaxis = list(
                  title = "TPE",
                  range = c(300, 2100),
                  tickfont = list(color = "white"),  # Set y-axis tick labels color to white
                  titlefont = list(color = "white"),  # Set y-axis title color to white
                  dtick = 200,  # Show tickmarks at intervals of 200
                  gridcolor = "rgba(255, 255, 255, 0.5)",  # Set gridline color to white with opacity
                  gridwidth = 1  # Set gridline width
                ),
                plot_bgcolor = "#333333",   # background color
                paper_bgcolor = "#333333",   # plot area background color
                showlegend = FALSE  # Hide legend (optional)
              ) |>
              plotly$config(
                displayModeBar = TRUE,  # Enable display of mode bar (optional, true by default)
                modeBarButtonsToRemove = list(
                  "zoom2d", "pan2d", "select2d",
                  "lasso2d", "zoomIn2d", "zoomOut2d",
                  "autoScale2d", "resetScale2d"
                ),
                displaylogo = FALSE  # Remove Plotly logo
              )
          }
        }
      )
  })
  
  output$playerHistory <- shiny$renderUI({
    promise_all(
      tpe = historyTPE(),
      bank = getBankHistory(data$pid),
      updates = getUpdateHistory(data$pid)
    ) |>
      then(
        onFulfilled = function(list){
          print(paste0("Rendering player history", Sys.time()))
          shiny$tabsetPanel(
            shiny$tabPanel(
              title = "TPE History",
              if(list$tpe |> is_empty()){
                NULL
              } else {
                list$tpe |>
                  dplyr$mutate(Time = as_datetime(Time)) |>
                  reactable(
                    columns =
                      list(
                        Time = colDef(format = colFormat(datetime = TRUE))
                      )
                  )
              }
            ),
            shiny$tabPanel(
              title = "Update History",
              if(list$updates |> is_empty()){
                NULL
              } else {
                list$updates |>
                  dplyr$mutate(Time = as_datetime(Time)) |>
                  reactable(
                    columns =
                      list(
                        Time = colDef(format = colFormat(datetime = TRUE))
                      )
                  )
              }
            ),
            shiny$tabPanel(
              title = "Bank History",
              if(list$bank |> is_empty()){
                NULL
              } else {
                list$bank |>
                  dplyr$mutate(Time = as_datetime(Time)) |>
                  reactable(
                    columns =
                      list(
                        Time = colDef(format = colFormat(datetime = TRUE)),
                        Transaction = colDef(format = colFormat(digits = 0, separators = TRUE, currency = "USD"))
                      )
                  )
              }
            )
          )
        }
      )
  })
  
  historyTPE <- shiny$reactive({
    pid <- input$selectedPlayer |>
      as.numeric()
    
    print("Getting TPE History")
    
    getTpeHistory(pid)
  }) |> 
    shiny$bindEvent(input$selectedPlayer)
}
