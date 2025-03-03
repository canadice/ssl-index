yourPlayerUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width = 12,
      fluidRow(
        playerInfoBoxUI(id = ns("playerInfo"))
      ),
      fluidRow(
        playerTPEBoxUI(id = ns("playerBuild")),
        playerOverviewBoxUI(id = ns("playerBuild")),
        playerUpdateBoxUI(id = ns("playerBuild")),
        box(
          title = "Updating History", collapsed = TRUE, collapsible = TRUE, width = NULL,
          fluidRow(
            column(width = 12,
                   reactableOutput(ns("historyUpdates")) |> 
                     withSpinnerMedium()
            )
          )
        ),
        box(title = "TPE History", collapsed = TRUE, collapsible = TRUE,width = NULL,
            fluidRow(
              column(
                width = 12,
                reactableOutput(ns("historyTPE"))
              )
            )
        ),
        br(),
        br()
      )
    )
  )
}

yourPlayerServer <- function(id, uid, parent, updated) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #### REACTIVES ####
      updating <- 
        reactiveVal({""})
      
      tpeTotal <- 
        reactiveVal({0})
      
      tpeBanked <- 
        reactiveVal({0}) 
      
      playerData <- 
        reactive({
          readAPI("https://api.simulationsoccer.com/player/getPlayer", query = list(uid = uid)) |> 
            future_promise()
        }) |> 
        bindEvent(
          updated()
        )
      
      
      historyTPE <- 
        reactive({
          playerData() |> 
            then(
              onFulfilled = function(value){
                getTpeHistory(value$pid)
              }
            )
        })
      
      historyUpdates <- 
        reactive({
          playerData() |> 
            then(
              onFulfilled = function(value){
                getUpdateHistory(value$pid)
              }
            )
        })
      
      #### OUTPUTS ####
      output$historyTPE <- renderReactable({
        historyTPE() |>
          then(
            onFulfilled = function(value){
              value |> 
              mutate(
                Time = as_datetime(Time)
              ) |> 
              reactable(
                columns = 
                  list(
                    Time = colDef(format = colFormat(datetime = TRUE))
                  )
              )
            },
            onRejected = NULL
          )
          
      })
        
      output$historyUpdates <- renderReactable({
        historyUpdates() |> 
          then(
            onFulfilled = function(value){
              value |> 
                mutate(
                  Time = as_datetime(Time)
                ) |> 
                reactable(
                  columns = 
                    list(
                      Time = colDef(format = colFormat(datetime = TRUE))
                    )
                )
            },
            onRejected = NULL
          )
      })
      
      #### OBSERVERS ####
      
      # Observer for the player boxes
      
      observe({
        playerData() |> 
          then(
            onFulfilled = function(value) {
              playerInfoBoxServer(id = "playerInfo", pid = value$pid, mainSession = parent)
            },
            onRejected = function(reason) {
              showToast("error", "An error occurred when loading your player. Please notify the BoD.")
            }
          )
      }) |> 
        bindEvent(playerData(), ignoreNULL = FALSE)
      
      ## Loading server functions 
      playerTPEBoxServer(id = "playerBuild", uid = uid, data = playerData, updated = updated, tpeTotal = tpeTotal, tpeBanked = tpeBanked)
      
      playerUpdateBoxServer(id = "playerBuild", uid = uid, data = playerData, tpeTotal = tpeTotal, tpeBanked = tpeBanked, updating = updating, updated = updated)
      
      playerOverviewBoxServer(id = "playerBuild", data = playerData, tpeTotal = tpeTotal, tpeBanked = tpeBanked, mainSession = parent)
      
      
    }
  )
}
