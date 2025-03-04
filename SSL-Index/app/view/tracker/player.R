box::use(
  bslib,
  dplyr,
  plotly[plotlyOutput, renderPlotly, plot_ly, config],
  promises[future_promise, then],
  reactable[colDef, colFormat, reactable, reactableOutput, renderReactable],
  shiny,
  shiny.router[get_query_param, change_page],
  stringr[str_to_upper],
  tippy[tippy],
)

box::use(
  app/logic/constant,
  app/logic/db/api[readAPI],
  app/logic/db/get[getUpdateHistory, getTpeHistory],
  app/logic/ui/reactableHelper[recordReactable, indexReactable],
  app/logic/ui/selector[leagueSelectInput],
  app/logic/ui/spinner[withSpinnerCustom],
  app/logic/ui/tags[flexCol, flexRow],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  
  shiny$tagList(
    bslib$card(
      bslib$card_header(
        bslib$layout_columns(
          col_widths = c(2, 4, 6),
          shiny$uiOutput(ns("selectPlayer")),
          shiny$uiOutput(ns("filterPlayer")),
          ""
        )
      ),
      bslib$card_body(
        bslib$layout_columns(
          col_widths = c(6, 6, 8, 4, 12),
          fillable = FALSE,
          # Col 6
          bslib$card(
            bslib$card_header(
              shiny$h3("Profile Information")
            ),
            bslib$card_body(
              bslib$layout_columns(
                col_widths = c(9, 3, 12),
                shiny$uiOutput(ns("playerName")),
                shiny$uiOutput(ns("clubLogo"), height = NULL),
                shiny$uiOutput(ns("playerInfo"))
              )
            )
          ),
          # Col 6
          bslib$card(
            bslib$card_header(
              shiny$h3("Recent Match Statistics")
            ),
            bslib$card_body(
              reactableOutput(ns("matchStatistics"))
            )
          ),
          # Col 8
          bslib$card(
            bslib$card_header(
              shiny$h3("Player Attributes")
            ),
            bslib$card_body(
              shiny$uiOutput(ns("playerAttributes"))
            )
          ), 
          # Col 4
          bslib$card(
            bslib$card_header(
              shiny$h3("Player Attributes")
            ),
            bslib$card_body(
              plotlyOutput(ns("tpeProgression"))
            )
          ),
          # Col 12...
          bslib$card(
            bslib$card_header(
              shiny$h3("Player History")
            ),
            bslib$card_body(
              shiny$uiOutput(ns("playerHistory"))
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
    ### Data
    playerData <- shiny$reactive({
      shiny$req(input$selectedPlayer)
      
      pid <- input$selectedPlayer |> 
        as.numeric()
      
      readAPI(url = "https://api.simulationsoccer.com/player/getPlayer", query = list(pid = pid)) |> 
        future_promise()
    })
    
    allPlayers <- shiny$reactive({
      readAPI(url = "https://api.simulationsoccer.com/player/getAllPlayers") |> 
        dplyr$select(name, pid, username, team, status_p) |> 
        future_promise()
    })
    
    historyTPE <- 
      shiny$reactive({
        shiny$req(input$selectedPlayer)
        pid <- input$selectedPlayer |> 
          as.numeric()
        
        getTpeHistory(pid)
      })
    
    historyUpdates <- 
      shiny$reactive({
        shiny$req(input$selectedPlayer)
        pid <- input$selectedPlayer |> 
          as.numeric()
        
        getUpdateHistory(pid)
      })
    
    historyBank <- 
      shiny$reactive({
        shiny$req(input$selectedPlayer)
        pid <- input$selectedPlayer |> 
          as.numeric()
        
        readAPI("https://api.simulationsoccer.com/bank/getBankTransactions",
                query = list(pid = pid)
        ) |> 
          future_promise()
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
    
    ### Output
    output$selectPlayer <- shiny$renderUI({
      shiny$req(input$retired, input$freeAgent)
      
      allPlayers() |> 
        then(
          onFulfilled = function(names) {
            names <- 
              names |>
              dplyr$filter(if(input$retired != 1) status_p > 0 else TRUE) |>
              dplyr$filter(if(input$freeAgent != 1) !(team %in% c("FA", "Retired")) else TRUE) |> 
              dplyr$arrange(name)
            
            namedVector <- names$pid
            
            names(namedVector) <- names$name
            
            shiny$selectInput(
              session$ns("selectedPlayer"), 
              "Select Player", 
              choices = namedVector,
              selected = query()
            )
          }
        )
    })
    
    output$filterPlayer <- shiny$renderUI({
      shiny$tagList(
        shiny$radioButtons(
          session$ns("retired"), 
          label = "Include retired: ", 
          choices = c("Yes" = 1, "No" = 0), 
          inline = TRUE
        ),
        shiny$radioButtons(
          session$ns("freeAgent"), 
          label = "Include free agents: ", 
          choices = c("Yes" = 1, "No" = 0), 
          inline = TRUE
        )
      )
    })
    
    ### Observe
    shiny$observe({
      shiny$req(input$selectedPlayer)
      
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
              shiny$tagList(
                bslib$layout_columns(
                  col_widths = c(6, 6),
                  
                )
              )
            })
            
          }
        )
    }) |> 
      shiny$bindEvent(input$selectedPlayer)
    
    
    
    
    
    shiny$observe({
      if(query() |> is.null()){
        change_page(paste0("tracker/player?pid=", input$selectedPlayer))
      } else {
        if (input$selectedPlayer != query()){
          change_page(paste0("tracker/player?pid=", input$selectedPlayer))
        }  
      }
    }) |> 
      shiny$bindEvent(input$selectedPlayer)
  })
}
