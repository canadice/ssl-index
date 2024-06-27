playerOverviewBoxUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(
      title = "Player Overview", collapsible = TRUE, width = NULL,
      fluidRow(
        column(
          width = 12,
          align = "right", 
          dropMenu(
            actionButton("go0", label = NULL, icon = icon("chevron-down")),
            uiOutput(
              outputId = ns("buttonRegression")
            ),
            uiOutput(
              outputId = ns("buttonUpdate")
            ),
            uiOutput(
              outputId = ns("buttonReroll")
            ),
            uiOutput(
              outputId = ns("buttonRedistribution")
            ),
            actionButton(ns("goToRetire"), label = "Retire"),
            placement = "left-end"
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          plotOutput(ns("playerOverview")) %>% 
            withSpinnerMedium()
        )
      )
    ) %>% 
      div(id = ns("attributeOverview"))
  )
}

playerOverviewBoxServer <- function(id, data, tpe) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #### BUTTONS ####
      
      output$buttonRegression <- renderUI({
        if(tpe$banked < 0) {
          actionButton(
            inputId = session$ns("goToRegression"),
            "Regress"
          )
        } else {
          actionButton(
            inputId = session$ns("goToRegression"),
            "Regress",
            disabled = ""
          )
        }
      })
      
      output$buttonUpdate <- renderUI({
        if(tpe$banked > 0) {
          actionButton(
            inputId = session$ns("goToUpdate"),
            "Update"
          )
        } else {
          actionButton(
            inputId = session$ns("goToUpdate"),
            "Update",
            disabled = ""
          )
        }
      })
      
      output$buttonReroll <- renderUI({
        # Rerolls can be made by users in their first two seasons in the SSL League proper
        check <- 
          ((data$class %>% str_extract(pattern = "[0-9]+") %>% as.numeric()) > (currentSeason$season - 2)) & 
          (data$rerollused == 0)
        
        if(check) {
          actionButton(
            inputId = session$ns("goToReroll"),
            "Reroll"
          )
        } else {
          # SHOW NOTHING
          
          # actionButton(
          #   inputId = session$ns("goToReroll"),
          #   "Reroll",
          #   disabled = ""
          # )
        }
      })
      
      output$buttonRedistribution <- renderUI({
        # Redistributions can be made by users in their first season in the SSL League proper
        check <- 
          ((data$class %>% str_extract(pattern = "[0-9]+") %>% as.numeric()) > (currentSeason$season - 1)) & 
          (data$redistused == 0)
        
        if(check) {
          actionButton(
            inputId = session$ns("goToRedist"),
            "Redistribute"
          )
        } else {
          # SHOW NOTHING
          
          # actionButton(
          #   inputId = session$ns("goToRedist"),
          #   "Redistribute",
          #   disabled = ""
          # )
        }
      })
      
      #### PLOT ####
      output$playerOverview <- renderPlot({
        p <- 
          data %>% 
          select(acceleration:throwing) %>% 
          select(where(~ !is.na(.x))) %>% 
          pivot_longer(
            cols = everything(),
            values_to = "Value",
            names_to = "Attribute"
          ) %>% 
          mutate(
            Attribute = str_to_title(Attribute)
          ) %>% 
          left_join(
            attributes,
            by = c("Attribute" = "attribute") 
          ) %>% 
          mutate(
            Attribute = factor(Attribute, levels = sort(Attribute %>% unique(), decreasing = TRUE)),
            group = factor(group, levels = c("Physical", "Mental", "Technical", "Goalkeeper")),
            ValueFill = case_when(
              Value >= 15 ~ 1,
              Value >= 10 ~ 2,
              TRUE ~ 3
            ) %>% factor()
          ) %>% 
          ggplot() + aes(x = Attribute, y = Value, fill = ValueFill) + 
          geom_bar(stat = "identity", color = "black") +
          facet_wrap(. ~ group, scales = "free", nrow = 1) + 
          scale_y_continuous(expand = c(0,0), limits = c(0, 20), minor_breaks = seq(0, 20, 1)) +
          scale_fill_manual(
            guide = NULL,
            values = c("#008450", "#EFB700", "#B81D13")
          ) +
          coord_flip() + 
          theme_bw() +
          theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_line(color = "gray50"),
            panel.grid.minor.x = element_line(color = "gray75"),
            axis.text = element_text(size = 14),
            axis.text.y = element_text(hjust = 0, margin = ggplot2::margin(l = 20, r = -110, unit = "pt"), color = "black", face = "bold"),
            strip.text = element_text(size = 16, face = "bold"),
            plot.margin = unit(margin(r = 10), "pt"),
            plot.background = element_rect(fill = "transparent", color = NA)
          ) + 
          labs(x = NULL, y = NULL)
        
        print(p)
      }, bg="transparent")
    }
  )
}