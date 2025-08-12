#Shiny App for Trackman Post-Game Pitching Reports

library(shiny)
library(tidyverse)
library(readr)
install.packages("shinythemes")
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Pitcher Report"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Trackman CSV", accept = ".csv"),
      uiOutput("pitcher_select")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary Table", tableOutput("debug_table")),
        tabPanel("Movement Plot", plotOutput("movement_plot")),
        tabPanel("Strikezone Plot", plotOutput("sz_plot")),
        tabPanel("Release Point Plot", plotOutput("rel_plot")),
        tabPanel("Whiff Chart", plotOutput("whiff_chart"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    df <- read_csv(input$file$datapath)
    print(names(df)) 
    df
  })
  
  output$pitcher_select <- renderUI({
    req(data())
    selectInput("pitcher", "Select Pitcher", choices = unique(data()$Pitcher))
  })
  
  pitcher_data <- reactive({
    req(input$pitcher)
    data() %>% filter(Pitcher == input$pitcher)
  })
  
  whiff_data <- reactive({
    req(pitcher_data())
    
    pitcher_data() %>%
      filter(PitchCall %in% c("SwingingStrike", "StrikeSwinging", "SwingMiss"))
  })
  
  output$movement_plot <- renderPlot({
    df <- pitcher_data()
    req(nrow(df) > 0)  
    
    ggplot(df, aes(x = HorzBreak, y = InducedVertBreak, color = `TaggedPitchType`)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      geom_point(alpha = 0.7, size = 3) +
      scale_color_brewer(palette = "Dark2") +
      labs(
        title = paste("Pitch Movement -", input$pitcher),
        x = "Horizontal Break",
        y = "Induced Vertical Break",
        color = "Pitch Type"
      ) +
      theme_minimal() +
      xlim(-30, 30) + ylim(-30, 30)
  })
  
  output$sz_plot <- renderPlot({
    df <- pitcher_data()
    req(nrow(df) > 0)
    
    home_plate <- data.frame(
      x = c(-0.708, 0.708, 0.708, 0, -0.708),
      y = c(0, 0, 0.216, 0.43, 0.216)
    )
    
    
    ggplot(df, aes(x = PlateLocSide, y = PlateLocHeight, color = `TaggedPitchType`)) +
      geom_point(alpha = 0.6, size = 3) +
      coord_fixed() +
      xlim(-4, 4) + ylim(0, 6) +
      labs(
        title = paste("Strike Zone Plot -", input$pitcher),
        x = "Horizontal Location (ft)",
        y = "Vertical Location (ft)"
      ) +
      annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5,
               alpha = 0.1, fill = "blue") +
      geom_polygon(data = home_plate, aes(x = x, y = y),
                   inherit.aes = FALSE,
                   fill = "white", color = "black")+
      theme_minimal()
  })
  output$debug_table <- renderTable({
    df <- pitcher_data() 
    req(nrow(df) > 0)
    
    df %>%
      group_by(`TaggedPitchType`) %>%
      summarise(
        Count = n(),
        Avg_Velocity = round(mean(RelSpeed, na.rm = TRUE), 1),
        Velocity_Max = round(max(RelSpeed, na.rm = TRUE), 1),
        Avg_Spin = round(mean(SpinRate, na.rm = TRUE), 0),
        IVB = round(mean(InducedVertBreak, na.rm = TRUE), 1),
        HB = round(mean(HorzBreak, na.rm = TRUE), 1)
      )
    
  })
  
  output$rel_plot <- renderPlot({
    df <- pitcher_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = RelSide, y = RelHeight, color = `TaggedPitchType`)) + 
      geom_point(size = 3) +  
      labs(title = "Pitcher Release Points by Pitch Type",
           x = "Release Side (feet)",
           y = "Release Height (feet)") +
      theme_minimal() +
      coord_fixed(ratio = 1, xlim = c(-4, 4), ylim = c(-.5, 8)) +  
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  
      scale_color_manual(values = c("red", "blue", "green", "orange", "purple", "brown", "pink"))
  })
  
  output$whiff_chart <- renderPlot({
    df <- whiff_data()
    req(nrow(df) > 0)
    
    home_plate <- data.frame(
      x = c(-0.708, 0.708, 0.708, 0, -0.708),
      y = c(0, 0, 0.216, 0.43, 0.216)
    )
    
    ggplot(df, aes(x = PlateLocSide, y = PlateLocHeight, color = `TaggedPitchType`)) +
      geom_point(alpha = 0.6, size = 3) +
      coord_fixed() +
      xlim(-2, 2) + ylim(0, 5) +
      labs(
        title = paste("Whiff Chart -", input$pitcher),
        x = "Horizontal Location (ft)",
        y = "Vertical Location (ft)"
      ) +
      annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5,
               alpha = 0.1, fill = "blue") +
      geom_polygon(data = home_plate, aes(x = x, y = y),
                   inherit.aes = FALSE,
                   fill = "white", color = "black")+
      theme_minimal()
  })
}


shinyApp(ui, server)
