box::use(
  bslib,
  dplyr,
  lubridate[as_date, as_datetime, floor_date, today],
  plotly,
  promises[future_promise, then, promise_all],
  reactable[colDef, colFormat, reactable, reactableOutput, renderReactable],
  rlang[is_empty],
  shiny,
  shiny.router[get_query_param, change_page],
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
  
    historyTPE <- shiny$reactive({
      shiny$req(input$selectedPlayer)

      pid <- input$selectedPlayer |>
        as.numeric()

      getTpeHistory(pid)
    })
    
    query <- shiny$reactive({
      shiny$req(allPlayers())
      pid <- get_query_param("pid")
      
      if (is.null(pid)){
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
            names <- 
              names |>
              dplyr$filter(if (input$retired != 1) status_p > 0 else TRUE) |>
              dplyr$filter(if (input$freeAgent != 1) !(team %in% c("FA", "Retired")) else TRUE) |> 
              dplyr$arrange(name)
            
            namedVector <- names$pid
            
            names(namedVector) <- names$name
            
            shiny$updateSelectInput(session = session, inputId = "selectedPlayer", choices = namedVector)
            
            change_page(paste0("tracker/player?pid=", names$pid[1]))
          }
        )
    }) |> 
      shiny$bindEvent(allPlayers(), once = TRUE)
    
    ### TODO NETWORK DOESN'T SEEM TO BE STARTING
    shiny$observe({
      playerData() |> 
        then(
          onFulfilled = function(data){
            output[["playerName"]] <- shiny$renderUI({
              shiny$tagList(
                shiny$h2(paste(data$name, paste0("(", data$class, ")"), sep = " ")),
                shiny$h3(paste0("@", data$username))
              )
            })
            
            output[["clubLogo"]] <- shiny$renderUI({
              shiny$img(
                src = sprintf("static/logo/%s.png", data$team),
                style = "height: 100px;",
                alt = data$team,
                title = data$team
              )
            })

            output[["playerInfo"]] <- shiny$renderUI({
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

            output[["matchStatistics"]] <- renderReactable({
              if (data$pos_gk == 20){
                matches <-
                  readAPI(
                    url = "https://api.simulationsoccer.com/index/keeperGameByGame",
                    query = list(name = data$name)
                  )

                if (!(matches |> is_empty())){
                  matches <-
                    matches |>
                    dplyr$select(1:8)
                }
              } else {
                matches <-
                  readAPI(
                    url = "https://api.simulationsoccer.com/index/outfieldGameByGame",
                    query = list(name = data$name)
                  )

                if (!(matches |> is_empty())){
                  matches <-
                    matches |>
                    dplyr$select(1:10)
                }
              }

              if (!(matches |> is_empty())){
                matches |>
                  dplyr$slice_head(n = 5) |>
                  recordReactable()
              } else {
                NULL
              }
            })

            output[["playerAttributes"]] <- shiny$renderUI({
              attributeReactable(data, session, output)
            })

            output[["tpeProgression"]] <- plotly$renderPlotly({
              historyTPE() |>
                then(
                  onFulfilled = function(tpe){
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

            output[["playerHistory"]] <- shiny$renderUI({
              promise_all(
                tpe = historyTPE(),
                bank = getBankHistory(data$pid),
                updates = getUpdateHistory(data$pid)
              ) |>
                then(
                  onFulfilled = function(list){
                    shiny$tabsetPanel(
                      shiny$tabPanel(
                        title = "TPE History",
                        list$tpe |>
                          dplyr$mutate(Time = as_datetime(Time)) |>
                          reactable(
                            columns =
                              list(
                                Time = colDef(format = colFormat(datetime = TRUE))
                              )
                          )
                      ),
                      shiny$tabPanel(
                        title = "Update History",
                        list$updates |>
                          dplyr$mutate(Time = as_datetime(Time)) |>
                          reactable(
                            columns =
                              list(
                                Time = colDef(format = colFormat(datetime = TRUE))
                              )
                          )
                      ),
                      shiny$tabPanel(
                        title = "Bank History",
                        list$bank |>
                          dplyr$mutate(Time = as_datetime(Time)) |>
                          reactable(
                            columns =
                              list(
                                Time = colDef(format = colFormat(datetime = TRUE)),
                                Transaction = colDef(format = colFormat(digits = 0, separators = TRUE, currency = "USD"))
                              )
                          )
                      )
                    )
                  }
                )
            })
          }
        )
    }) |> 
      shiny$bindEvent(playerData())
    
    
    shiny$observe({
      if((query() |> is.null())){
        # change_page(paste0("tracker/player?pid=", input$selectedPlayer))
      } else {
        if (input$selectedPlayer != query()){
          change_page(paste0("tracker/player?pid=", input$selectedPlayer))
        }  
      }
    }) |> 
      shiny$bindEvent(input$selectedPlayer)
  })
}
