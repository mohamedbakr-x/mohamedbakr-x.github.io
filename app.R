library(shiny)
library(fmsb)

# Define the UI
ui <- fluidPage(
  

  
  titlePanel("Player Performance Radar Chart"),
  sidebarLayout(
    sidebarPanel(
      selectInput("player", "Select a player:", choices = players_data$Player),
      hr(),
      helpText("Select a player from the dropdown menu to view their radar chart.")
    ),
    
    mainPanel(
      plotOutput("radarChart")
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Create a reactive expression for the selected player data
  selected_player_data <- reactive({
    subset(players_data, Player == input$player, select = c("Gls", "Ast", "DrbSucc", "TklW", "Int", "PrgC"))
  })
  
  # Create a reactive expression for the radar chart data
  radar_data <- reactive({
    player_data <- selected_player_data()
    max_values <- c(max(players_data$Gls, na.rm = TRUE), max(players_data$Ast, na.rm = TRUE),
                    max(players_data$DrbSucc, na.rm = TRUE), max(players_data$TklW, na.rm = TRUE),
                    max(players_data$Int, na.rm = TRUE), max(players_data$PrgC, na.rm = TRUE))
    rbind(max_values, min_values, player_data)
  })
  
  # Generate the radar chart
  output$radarChart <- renderPlot({
    radar_data <- radar_data()
    
    # Set labels
    labels <- c("Goals", "Assists", "Successful Dribbles", "Tackles Won", "Interceptions", "Progressive Carries")
    
    # Set title
    title <- paste(input$player, "Performance Radar Chart")
    
    # Set color and transparency
    color <- "red"
    
    par(bg = "#9fb8d7")
    
    # Plot the radar chart
    radarchart(radar_data,
               axistype = 1,
               pcol = color,
               pfcol = scales::alpha(color, 0.6),
               plwd = 2,
               plty = 1,
               vlabels = labels,
               title = title,
               cglcol = "grey", 
               cglty = 1, 
               cglwd = 0.8,
               caxislabels = c("", "", "", "", ""),
               vlcex = 0.7)
  })
}

# Run the Shiny app
shinyApp(ui, server)
