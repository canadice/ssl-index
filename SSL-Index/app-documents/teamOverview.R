
############################################################################
############################################################################
###                                                                      ###
###         TOOL TO VISUALIZE DISTRIBUTION OF PLAYERS ATTRIBUTES         ###
###                                                                      ###
############################################################################
############################################################################


##---------------------------------------------------------------
##                  Creating the user interface                 -
##---------------------------------------------------------------

teamOverviewUI <- function(id){
  ns <- NS(id)
  tagList(
    
    ##------------------------------------------
    ##  Settings for the background team image  
    ##------------------------------------------
    tags$style(
      HTML('
         #buttons {
         background-color:#f3f3f300; position:fixed; margin-bottom:50px; opacity:1; height:50px; z-index:5;
         display: flex;
         align-items: center;
         justify-content: center;
         }

         ')
    ),
    
    ##---------------
    ##  The content  
    ##---------------
    
    fluidRow(
      column(
        width = 12,
        div(
          id = ns("content"),
          column(
            width = 12,
            div(
              br(),
              imageOutput(
                outputId = ns("logo"),
                height = "100%"
              ),
              style=
              "position: relative;
              margin: auto;",
              align = "center",
              div(
                style = 
                "position: absolute;
                top: 0px;
                left: 0px;
                width: 100%;",
                align = "left",
                fluidRow(
                  column(
                    width = 4,
                    offset = 1,
                    uiOutput(outputId = ns("selectTeams"))
                  )
                ),
                fluidRow(
                  column(
                    width = 10,
                    offset = 1,
                    id = ns("teamTabBox"),
                    uiOutput(outputId = ns("dataTableCSS")),
                    tabBox(
                      width = NULL,
                      id = ns("tabBox"),
                      tabPanel(
                        title = "Current Roster",
                        id = ns("roster"),
                        DTOutput(outputId = ns("dataTableRoster")),
                        
                      ),
                      tabPanel(
                        title = "TPE Visualizer",
                        id = ns("tpeVisualizer"),
                        column(
                          width = 8,
                          offset = 2,
                          h5("TPE Distribution", align = "center"),
                          p("The following visualization shows the TPE distribution of the chosen team.",
                            "Hovering over each bar shows the class distribution of players within the TPE interval."),
                          plotlyOutput(
                            outputId = ns("teamTPECharts")
                          )  
                        )
                      ),
                      tabPanel(
                        title = "Role and Duties",
                        column(
                          width = 4,
                          plotlyOutput(
                            outputId = ns("positionPicker"),
                            width = "250px",
                            height= "225px"
                          ),
                          uiOutput(
                            outputId = ns("rolePicker")
                          )
                        ),
                        column(
                          width = 8,
                          DTOutput(
                            outputId = ns("roleDataTable")
                          )
                        )
                      )
                    )
                  )
                )
              ) %>% 
                fluidRow()
            )
          )
        ) %>% 
          fluidRow()
      )
    )
  )
}


## Backend for vizualizations
teamOverviewSERVER <- function(id){
  ## Calling moduleServer function
  moduleServer(
    id,
    function(input, output, session){
      
      circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
        r = diameter / 2
        tt <- seq(0,2*pi,length.out = npoints)
        xx <- center[1] + r * cos(tt)
        yy <- center[2] + r * sin(tt)
        return(data.frame(x = xx, y = yy))
      }
      
      ##---------------------------------------------------------------
      ##                    Settings for datatables                   -
      ##---------------------------------------------------------------
      
      teamTableOptions <- 
        list(
          orderClasses = FALSE, 
          ## Sets a scroller for the rows
          scrollY = '500px',
          ## Sets size of rows shown
          scrollCollapse = TRUE,
          ## Removes pages in the table
          paging = FALSE,
          ## Adds scrollable horizontal
          scrollX = TRUE,
          # pageLength = 10,
          # lengthMenu = c(10, 25, 50, 100),
          dom = 'ft',
          bInfo = FALSE#,
          ## Defines which columns the table shows by default
          # columnDefs = 
          #   list(
          #     list(
          #       targets = 0,
          #       orderData = 6
          #     ),
          #     list(
          #       targets = 3,
          #       orderData = 7
          #     ),
          #     list(
          #       targets = c(6:7),
          #       visible = FALSE
          #     )
          #   )
        )
      ##----------------------------------------------------------------
      ##                    Observers for the page                     -
      ##----------------------------------------------------------------
      
      ### Observes when a team is selected
      observeEvent(
        {
          input$selectedTeam
        },
        {
          ### Updates the player data to the active team
          SSLData %>% 
            filter(
              Team == input$selectedTeam
            ) %>% 
            teamData()
          
          ### Updates the team colors to the selected team
          teamInfo %>% 
            filter(
              team == input$selectedTeam
            ) %>% 
            select(color.primary) %>% 
            unname() %>% 
            teamColor()
        }
      )
      
      ##----------------------------------------------------------------
      ##          Readies the data used for the vizualisations         -
      ##----------------------------------------------------------------
      
      SSLData <- 
        playerData %>% 
        mutate(
          TPEbins = 
            cut(
              TPE, 
              breaks = c(350,425,550,700,1000,1400,1800,2500),
              labels = 
                c("350-425",
                  "426-550",
                  "551-700",
                  "701-1000",
                  "1001-1400",
                  "1401-1800",
                  "1800+"),
              right = FALSE
            )
        )
      
      ##----------------------------------------------------------------
      ##                          UI Outputs                           -
      ##----------------------------------------------------------------
      
      ### Outputs the team selector
      output$selectTeams <- 
        renderUI({
            selectInput(
              inputId = session$ns("selectedTeam"),
              label = "Select a team",
              choices = SSLData %>% select(Team) %>% unique()
            )  
        })
      
      ### Changes the CSS values of the datatable
      output$dataTableCSS <- 
        renderUI({
          tags$style(
            HTML(
              ### Kenvald you beautiful specimen!
              "
              #teamOverview-teamTabBox .nav-tabs-custom,
              #teamOverview-teamTabBox .nav-tabs-custom>.tab-content, 
              #teamOverview-teamTabBox .nav-tabs-custom>.nav-tabs
              {background: #FFFFFF00;}
              ",
              ## The use of !important is important to overwrite site default css values for dataTables
              ## #id of the datatable only changes it for this module and not the entire datatable format
              paste0("#teamOverview-teamTabBox tbody tr.odd, 
                    #teamOverview-teamTabBox tbody tr.odd 
                    {background-color:", paste0(teamColor(), 25), "!important}
                    "),
              paste0("#teamOverview-teamTabBox tbody tr
                     {background-color:", paste0("#FFFFFF", 10), "!important}")
            )
          )
        })
      
      ##---------------------------------------------------------------
      ##                      Datatable Outputs                       -
      ##---------------------------------------------------------------
      
      output$dataTableRoster <- DT::renderDT({
        if(teamData() %>% is.null()){
          NULL
        } else{
          teamData() %>% 
            select(
              Name,
              Class,
              Position,
              Username,
              TPE#,
              # Active,
              # Roster,
              # Prospect,
            ) %>%
            arrange(
              # Active,
              -TPE
            ) %>% 
            datatable(
              rownames = FALSE,
              escape = FALSE,
              selection = "none",
              options = teamTableOptions
            )
        }
      }
      )
      
      ##---------------------------------------------------------------
      ##                        Image Outputs                         -
      ##---------------------------------------------------------------
      
      output$logo <- 
        renderImage({
          
          if(input$selectedTeam %>% is.null()){
            tempImage <- 
              image_blank(width = 200, height = 200) %>% 
              image_write(tempfile(fileext = "png"), format = "png")
          } else {
            visData <- 
              teamInfo %>% 
              filter(
                team == input$selectedTeam
              ) 
            
            tempImage <- 
              image_draw(
                image_read(visData$logo),
                xlim = c(0,1),
                ylim = c(0,1)
              ) %>%
              image_resize("x600") %>% 
              image_colorize(opacity = 90, color = "white") %>% 
              image_write(tempfile(fileext = "png"), format = "png")
            
            dev.off()
          }
          
          list(src = tempImage, contentType = "image/png")
        },
        deleteFile = TRUE)
      
      
      ##---------------------------------------------------------------
      ##              Visualization of TPE Distribution               -
      ##---------------------------------------------------------------
      
      output$teamTPECharts <- renderPlotly({
        
        data <- 
          teamData() %>% 
          group_by(TPEbins, Class) %>% 
          mutate(
            classCount = n()
          ) 
          
        classText <- 
          data %>% 
          group_by(TPEbins) %>% 
          summarize(
            binCount = n(),
            text = paste(Class, classCount, sep = ": ") %>% unique() %>% sort() %>% paste(collapse = "\n")
          )
        
        visData <- 
          data %>% 
          left_join(
            classText,
            by = c("TPEbins")
          ) %>% 
          ungroup() %>% 
          select(
            TPEbins,
            binCount,
            text
          ) %>% 
          unique()
        
        p <- 
          ggplot(visData) + 
          aes(x = TPEbins, y = binCount, text = text) +
          geom_col(
            color = "black",
            fill = paste(teamColor()),
            width = 1
          ) + 
          labs(y = "Count", x = "TPE") + 
          theme_minimal() + 
          scale_x_discrete(drop = FALSE) + 
          scale_y_continuous(
            expand = expansion(add = c(0, 1)),
            breaks = seq(0, 100, 2)
          ) +
          theme(
            panel.spacing.y = unit(1.5, "lines"),
            strip.background.y = element_rect(fill = "white"),
            strip.text.y = element_text(angle = 0, size = 12), 
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major = element_line(color = "gray80"),
            panel.grid.minor = element_line(color = "gray60"),
            panel.background = element_rect(fill = "transparent"), 
            axis.line.y = element_line(color = "black"),
            axis.line.x = element_line(color = "black"),
            plot.background = element_rect(fill = "transparent", color = "black")
          ) + 
          annotate("segment", x=-Inf, xend=Inf, y=0, yend=0)
        
        p %>% 
          ggplotly(tooltip = "text") %>% 
          layout(hoverlabel=list(bgcolor="white")) %>% 
          config(
            displayModeBar = FALSE
          )
          
      })
      
      ##---------------------------------------------------------------
      ##              Visualization of position ability               -
      ##---------------------------------------------------------------
      
      output$positionPicker <- renderPlotly({
        p <- 
          ggplot(positionalCoord) + aes(x = x, y = y) +
          ## Pitch rectangles
          geom_rect(xmin = 25,xmax = 725,ymin = -25,ymax = 925,color = "white",fill = "#0A8043",size = 0.5) +
          geom_rect(xmin = 50,xmax = 700, ymin = 0,ymax = 900,color = "white",fill = "#78B833",size = 0.5) +
          ## Inner keeper box
          geom_rect(xmin = 300,xmax = 450,ymin = 0,ymax = 50,color = "white",fill = NA) +
          geom_rect(xmin = 300,xmax = 450,ymin = 850,ymax = 900,color = "white",fill = NA) +
          ## Outer keeper box
          geom_rect(xmin = 200,xmax = 550,ymin = 0,ymax = 150,color = "white",fill = NA) +
          geom_rect(xmin = 200,xmax = 550,ymin = 750,ymax = 900,color = "white",fill = NA) +
          ## Half-line
          geom_segment(aes(x = 50,xend = 700,y = 450,yend = 450),size = 1,color = "white") +
          ## Central circle
          geom_path(aes(x, y), data = circleFun(c(375, 450), 100), color = "white", size = 1) +
          geom_point(aes(x = 375, y = 450), size = 5, color = "white") +
          geom_point(aes(text = Position), size = 4) + 
          coord_flip() + 
          scale_x_continuous(
            breaks = NULL,
            limits = c(0, 750)
          ) + 
          scale_y_continuous(
            breaks = NULL,
            limits = c(0, 900)
          ) + 
          labs(x = "", y = "") +
          theme(
            plot.background = element_rect(fill = NA),
            panel.background = element_rect(fill = NA),
            panel.grid = element_blank(),
            axis.ticks = element_blank()
          )
        
        p %>% 
          ggplotly(tooltip = c("text"), source = "position") %>% 
          layout(
            xaxis = 
              list(showticklabels = FALSE,
                   fixedrange = TRUE), 
            yaxis = 
              list(
                showticklabels = FALSE,
                fixedrange = TRUE,
                title = list(standoff = 0, font = list(size = 1))
                ),
            margin = list(r = 0, l = 0, t = 0, b = 0, pad = 0),
            hoverlabel=list(bgcolor="white")
          ) %>% 
          config(
            displayModeBar = FALSE
          )
          
      })
      
      output$rolePicker <- renderUI({
        eventData <- event_data("plotly_click", source = "position")
        
        if(eventData %>% length() != 0){
          selectedPosition <- 
            positionalCoord %>%
            filter(
              x == eventData$y,
              y == eventData$x
            ) %>%
            select(
              Position
            ) 
          
          position(selectedPosition)
          
          roles <- 
            colnames(playerData) %>% 
            str_replace_all(
              pattern = "\\.",
              replacement = " "
            ) %>% 
            str_detect(
              pattern = paste("roleAbility", selectedPosition %>% as.character(), sep = " ")
            ) %>% 
            which()
          
          roleNames <- 
            colnames(playerData)[roles] %>% 
            str_replace_all(
              pattern = "\\.",
              replacement = " "
            ) %>% 
            str_remove_all(
              pattern = 
                paste(
                  "roleAbility", 
                  selectedPosition %>% as.character(), 
                  sep = " "
                )
            )
          
          availableRoles <- 
            colnames(playerData)[roles]
          
          names(availableRoles) <- roleNames
          
          tagList(
            h3(selectedPosition),
            selectInput(
              inputId = session$ns("rolePicker"),
              choices = 
                availableRoles,
              label = "Select the role and duty for the position"
            )
          )
        }
        
        
        
        
      })
      
      output$roleDataTable <- renderDT({
        if(input$rolePicker %>% is.null()){
          
        } else {
          tableData <- 
            teamData() %>% 
            select(
              Name,
              `Preferred Foot`,
              `Preferred Position`,
              input$rolePicker
            ) %>% 
            arrange(
              across(input$rolePicker, desc)
            ) %>% 
            mutate(
              across(
                input$rolePicker,
                ~ sapply(
                    X = (.x %/% 50) + 1,
                    FUN = function(x) {
                      replicate(
                        x,
                        expr = icontext("star") %>% as.character()    
                      ) %>% 
                        paste0(collapse = "")
                    }
                  )  
                  
              )
            )
          
          colnames(tableData) <- 
            c(
              "Name",
              "Preferred\nFoot",
              "Preferred\nPosition",
              input$rolePicker %>% 
                str_replace_all(
                  pattern = "\\.",
                  replacement = " "
                ) %>% 
                str_remove_all(
                  pattern = 
                    paste(
                      "roleAbility", 
                      position() %>% as.character(), 
                      sep = " "
                    )
                )
            )
          
          tableData %>% 
            datatable(
              rownames = FALSE,
              escape = FALSE,
              selection = "none",
              options = 
                list(
                  orderClasses = FALSE, 
                  ## Removes pages in the table
                  paging = FALSE,
                  dom = 't',
                  bInfo = FALSE
                )
            )  
        }
      })
      
      ##------------------------------------------------------------------------
      ##  Initialize reactiveValues that will be used as interactive options   -
      ##------------------------------------------------------------------------
      teamData <- reactiveVal(NULL)
      
      teamColor <- reactiveVal(NULL)
      
      position <- reactiveVal(NULL)
    }
  )
}

