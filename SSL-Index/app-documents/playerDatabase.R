
###########################################################################
###########################################################################
###                                                                     ###
###                         PLAYER BUILDER TOOL                         ###
###                                                                     ###
###########################################################################
###########################################################################


### UI module for player similarities using MDS
playerDatabaseUI <- function(id){
  
  ## Creating the namespacing function for all iDs
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(
        tabBox(
          width = NULL,
          tabPanel(
            "Player Updater",
            
            fluidRow(
              ##-----------------------------------
              ##  Left hand side with information  
              ##-----------------------------------
              column(
                width = 4,
                box(
                  title = "Information",
                  status = "info",
                  solidHeader = TRUE,
                  width = NULL,
                  selectInput(
                    inputId = ns("player"),
                    label = "Select a player",
                    choices = 
                      playerData %>% 
                      select(Team, Name) %>% 
                      group_by(Team) %>% 
                      arrange(Name) %>% 
                      group_split(.keep = FALSE) %>% 
                      setNames(playerData$Team %>% unique() %>% sort()) %>% 
                      lapply(
                        X = .,
                        FUN = function(x){
                          c(x) %>% 
                            unname() %>% 
                            lapply(
                              X = .,
                              FUN = function(x){
                                as.list(x)
                              }
                            ) %>% 
                            unlist(recursive = FALSE)
                        }
                      )
                  ),
                  h4("TPE Information", align = "center"),
                  fluidRow(
                    column(
                      width = 10,
                      offset = 1,
                      uiOutput(
                        outputId = ns("earnedTPE")
                      ),
                      uiOutput(
                        outputId = ns("usedTPE")
                      )
                    )
                  ),
                  br(),
                  h5("Update Scale" %>% strong(), align = "center"),
                  DTOutput(
                    outputId = ns("costTPE"),
                    width = "80%"
                  ) %>% 
                    div(align = "center"),
                  actionButton(
                    inputId = ns("exportButton"),
                    label = "Export",
                    width = "80%"
                  ) %>%
                    div(align = "center")
                )
              ),
              
              ##------------------------------------
              ##  Right hand side with the builder  
              ##------------------------------------
              column(
                width = 8,
                p("Edit the Value column by double clicking on the cell of the attribute you want to edit. 
                  Clicking on export provides a formatted text with all the updates you have made on your build."),
                DTOutput(ns("attributeTable"), width = "80%")
              )
              ##------------------------------------
            )
          ),
          tabPanel(
            "Draft Class Tracker",
            fluidRow(
              column(
                width = 2,
                box(
                  title = "Information",
                  status = "info",
                  solidHeader = TRUE,
                  width = NULL,
                  selectInput(
                    inputId = ns("class"),
                    label = "Select Draft Class",
                    choices = c(
                      "ALL",
                      unique(playerData$Class) %>% sort(decreasing = TRUE))
                  )
                )
              ),
              column(
                width = 10,
                box(
                  title = "Tracker",
                  status = "primary",
                  solidHeader = TRUE,
                  width = NULL,
                  DT::DTOutput(
                    outputId = ns("tableTPE")
                  )
                )
              )
            )
          )
        )
      )
    )
 )
}

## Backend module for player similarities
playerDatabaseSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    
    ## Definining the mechanisms
    function(input, output, session){
      
      
      ##---------------------------------------------------------------
      ##                        Reactive values                       -
      ##---------------------------------------------------------------
      
      ## Loads selected data for TPE Tracker
      currentTPEData <- reactive({
        if(input$class != "ALL"){
          playerData <- 
            playerData %>% 
            filter(
              Class == input$class
            )  
        }
        
        playerData %>% 
          select(
            Name,
            Username,
            Class,
            Team,
            `Preferred Position`,
            TPE,
            # `Applied TPE` = TPE - `TPE Available`,
            Active
          ) %>% 
          left_join(
            teamInfo %>% 
              select(
                team, 
                color.primary,
                color.secondary
              ),
            by = c("Team" = "team")
          )
        
      })
      
      currentAvailable <- reactiveVal(350)
      
      currentCost <- reactiveVal(0)
      
      ## Reactive data set that updates the input build of a player
      reactives <- reactiveValues(
        currentBuild = {
        playerData %>% 
          filter(Name == input$player) %>% 
          select(
            Acceleration:Throwing
          ) %>% 
          pivot_longer(
            cols = everything(),
            names_to = "Attribute",
            values_to = "Value"
          ) %>% 
          left_join(
            tpeCost,
            by = c("Value" = "value")
          ) %>% 
          mutate(
            cost = if_else(Attribute %in% c("Natural Fitness", "Stamina"), 0, cost)
          ) %>% 
          left_join(
            attributes,
            by = "Attribute"
          ) %>% 
          filter(
            !is.na(Value)
          ) %>% 
          select(
            -Keeper
          )
      })
      
      ##---------------------------------------------------------------
      ##                          Observers                           -
      ##---------------------------------------------------------------
      
      ## Observer that calculates the correct used TPE and available TPE for the build
      observeEvent(
        input$currentTPE,
        {
          currentAvailable(input$currentTPE - sum(reactives$currentBuild$cost))
          currentCost(sum(reactives$currentBuild$cost))
        },
        ignoreInit = FALSE
      )
      
      ## Observes a change in player and updates the current build and costs accordingly
      observeEvent(
        input$player,
        {
          reactives$currentBuild <- 
            playerData %>% 
            filter(Name == input$player) %>% 
            select(
              Acceleration:Throwing
            ) %>% 
            pivot_longer(
              cols = everything(),
              names_to = "Attribute",
              values_to = "Value"
            ) %>% 
            left_join(
              tpeCost,
              by = c("Value" = "value")
            ) %>% 
            mutate(
              cost = if_else(Attribute %in% c("Natural Fitness", "Stamina"), 0, cost)
            ) %>% 
            left_join(
              attributes,
              by = "Attribute"
            ) %>% 
            filter(
              !is.na(Value)
            ) %>% 
            select(
              -Keeper
            )
          
          currentAvailable(input$currentTPE - sum(reactives$currentBuild$cost))
          currentCost(sum(reactives$currentBuild$cost))
        }
      )
      
      observeEvent(
        input$attributeTable_cell_edit,
        {
          editedCell = input$attributeTable_cell_edit
          i = as.numeric(editedCell$row)
          j = as.numeric(editedCell$col)
          k = as.numeric(editedCell$value)
          
          # Checks if new value falls within attribute constraints
          if(k < 5){ 
            k <- 5
          } else if(k > 20) {
            k <- 20
          }
          
          # Checks if you alter the Natural Fitness or Stamina which are fixed attributes
          if(i %in% c(5,7)){
            k <- 20 
            
            reactives$currentBuild[i,j] <- k
          } else {
            #write values to reactive
            reactives$currentBuild[i,j] <- k
            
            reactives$currentBuild[i,"cost"] <- tpeCost$cost[tpeCost$value == k]  
          }
          
          currentAvailable(input$currentTPE - sum(reactives$currentBuild$cost))
          currentCost(sum(reactives$currentBuild$cost))
        }
      )
      
      
      ## Observer for the export button that creates a formatted text with all info of the build
      observeEvent(
        input$exportButton,
        {
          current <- 
            playerData %>% 
            filter(Name == input$player) %>% 
            select(
              Acceleration:Throwing
            ) %>% 
            pivot_longer(
              cols = everything(),
              names_to = "Attribute",
              values_to = "Value"
            ) %>% 
            left_join(
              tpeCost,
              by = c("Value" = "value")
            ) %>% 
            mutate(
              cost = if_else(Attribute %in% c("Natural Fitness", "Stamina"), 0, cost)
            ) %>% 
            left_join(
              attributes,
              by = "Attribute"
            ) %>% 
            filter(
              !is.na(Value)
            ) %>% 
            select(
              -Keeper
            )
          
          updated <- 
            reactives$currentBuild
          
          merged <- 
            current %>% 
            left_join(
              updated,
              by = "Attribute",
              suffix = c(".old", ".new")
            ) %>% 
            filter(
              Value.old != Value.new
            ) %>% 
            mutate(
              ChangeCost = cost.new - cost.old
            ) %>% 
            select(
              Attribute,
              Value.old,
              Value.new,
              ChangeCost
            )  
          
          if(merged %>% nrow() > 0){
            showModal(
              modalDialog(
                span(
                  "Copy the code below containing your updates into your update post."
                ),
                br(),
                br(),
                column(
                  width = 8,
                  offset = 2,
                  helpText(
                    paste(
                      paste(
                        paste(
                          "[b]Earned TPE:[/b] ", 
                          playerData %>% filter(Name == input$player) %>% select(TPE) %>% unlist() %>% unname(),
                          "+",
                          input$currentTPE-playerData %>% filter(Name == input$player) %>% select(TPE) %>% unlist() %>% unname(),
                          "=",
                          input$currentTPE),
                        paste("[b]Used TPE:[/b] ", currentCost()), 
                        paste("[b]Banked TPE:[/b] ", currentAvailable()),
                        sep = "<br>"
                      ),
                      " ",
                      paste(
                        apply(
                          X = merged,
                          MARGIN = 1, 
                          FUN = function(merged){
                            paste(merged[1], paste(merged[2:3], collapse = " -> "), paste("(",merged[4], ")", sep = ""), collapse = " ")    
                          }
                        ),
                        collapse = "<br>"
                      ),
                      sep = "<br>"
                    ) %>% 
                      HTML()
                  ) %>% 
                    div(
                      style = "background: #f0f0f0; border: #656565"
                    )
                ),
                title="Update output",
                footer = 
                  tagList(
                    modalButton("Ok")
                  ),
                easyClose = TRUE
              )
            )
          } else {
            showModal(
              modalDialog(
                span(
                  "You have not changed your build yet."
                ),
                title="Update output",
                footer = 
                  tagList(
                    modalButton("Ok")
                  ),
                easyClose = TRUE
              )
            )
          }
          
        }
      )
      
      ##---------------------------------------------------------------
      ##                      Datatable outputs                       -
      ##---------------------------------------------------------------
      
      ## Table with the cost of all attribute values
      output$costTPE <- renderDT({
        datatable(
          data.frame(
            `Value` = c("5-7", "8-10", "11-13", "14-16", "17-18", "19-20"),
            Cost = c(2, 4, 6, 12, 18, 25)
          ),
          class = 'compact cell-border stripe',
          rownames = FALSE,
          style = "bootstrap",
          escape = FALSE,
          options = 
            list(
              ordering = FALSE,
              dom = 't',
              ## Sets color of table background
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#00044d', 'color': '#fff'});",
                "}")
            )
        )  
      })
      
      ## TPE information UI
      output$usedTPE <- renderUI({
        tagList(
          fluidRow(
            column(
              width = 6,
              h5("Used TPE")
            ),
            column(
              width = 6,
              h5(currentCost())
            )
          ),
          fluidRow(
            column(
              width = 6,
              h5("Available TPE")
            ),
            column(
              width = 6,
              h5(currentAvailable())
            )
          ),
          fluidRow(
            column(
              width = 6,
              h5("Forum TPE")
            ),
            column(
              width = 6,
              h5(playerData %>% filter(Name == input$player) %>% select(TPE) %>% unlist() %>% unname())
            )
          )
        )
      })
      
      output$earnedTPE <- renderUI({
        tagList(
          fluidRow(
            column(
              width = 6,
              h5("Earned TPE")
            ),
            column(
              width = 6,
              numericInput(
                inputId = session$ns("currentTPE"),
                label = NULL,
                value = playerData %>% filter(Name == input$player) %>% select(TPE) %>% unlist() %>% unname(),
                min = 350,
                max = 2500,
                width = "80%"
              )
            )
          )
        )
      })
      
      ## js function for automatic reranking
      js <- c(
        "table.on('draw.dt', function(){",
        "  var PageInfo = table.page.info();",
        "  table.column(0, {page: 'current'}).nodes().each(function(cell,i){", 
        "    cell.innerHTML = i + 1 + PageInfo.start;",
        "  });",
        "})")
      
      ## TPE Tracker for different classes
      output$tableTPE <- renderDT({
        datatable(
          currentTPEData() %>% 
            arrange(
              -TPE
            ), 
          callback = JS(js),
          style = "bootstrap",
          class = 'compact cell-border stripe',
          rownames = TRUE,
          escape = FALSE,
          options = 
            list(
              ordering = TRUE, 
              ## Sets a scroller for the rows
              scrollX = '800px',
              scrollY = '550px',
              ## Sets size of rows shown
              scrollCollapse = TRUE,
              paging = FALSE,
              dom = 'ft',
              columnDefs = 
                list(
                  list(
                    targets = 8:9,
                    visible = FALSE
                  )
                )
            )
        ) %>% 
          formatStyle(
            columns = 0:7,
            valueColumns = "color.primary",
            backgroundColor = 
              styleEqual(
                sort(unique(teamInfo$color.primary)), 
                sort(unique(teamInfo$color.primary))
              )
          ) %>% 
          formatStyle(
            columns = 0:7,
            valueColumns = "color.secondary",
            color = 
              styleEqual(
                sort(unique(teamInfo$color.secondary)), 
                sort(unique(teamInfo$color.secondary))
              )
          )
      })
      
      ##----------------------------------------------------------------
      ##              Datatable output for attributes                  -
      ##----------------------------------------------------------------
      
      output$attributeTable <- renderDT({
        datatable(
          reactives$currentBuild %>% 
            relocate(
              Group,
              .before = Attribute
            ) %>% 
            rename(
              Cost = cost
            ), 
          rownames = FALSE,
          selection = "none",
          editable = 
            list(
              target = "cell",
              disable = 
                list(
                  columns = c(0,1,3)
                )
            ),
          extensions = "RowGroup",
          options = 
            list(
              rowGroup = list(datSrc = 0),
              ordering = FALSE, 
              pageLength = -1,
              dom = 'Rt',
              ## Sets color of table background
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#00044d', 'color': '#fff'});",
                "}")
            )
          )
      })
      
      
    }
  )
}

