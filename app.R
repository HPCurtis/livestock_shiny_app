# Import pre-requiste packages for data and forecasts.
library(fpp3)
library(shiny)

# Import Australian livestock data.
df<- aus_livestock

# Add year column.
df$year <- as.integer(substr(df$Month, 1, 4))

# Define UI for the app
ui <- fluidPage(
  # App title
  titlePanel("Australian Livestock forecasts"),
  
  # Define side panel and following options to vary the data plotted.
  sidebarLayout(
    
    # All sidebar widgets
    sidebarPanel(
      # Option to vary the type of livestock plotted.
      selectInput(inputId = "livestock_id", label = "Livestock", choices = unique(df$Animal)),
      # Option to vary the australian state for which the data is from.
      selectInput(inputId = "state_id", label = "State", choices = unique(df$State)),
      # Option vary the years plotted.
      sliderInput("yearRange",
                  label = "Select Year Range",
                  min = min(df$year),
                  max = max(df$year),
                  value = c(min(df$year), max(df$year)),
                  step = 1,
                  sep = ""),
      # Option to vary the training dat used to train models and forecasts generated.
      sliderInput("trainRange",
                  label = "Select Trianing data Range",
                  min = min(df$year),
                  max = max(df$year),
                  value = c(min(df$year), max(df$year)),
                  step = 1,
                  sep = ""),
      
      numericInput("forecasts_range", label = "Forecast range",
                   value = 2, 
                   min = 2,
                   max = 30)
    ),
    
    # UI for the main panel specifically the plot of the time-series data. 
    mainPanel(
      plotOutput("timeseries_plot")
    )
  )
)

# Define server side functionality.
server <- function(input, output) {
  output$timeseries_plot <- renderPlot({
    filtered_df <- df %>%
    filter(Animal == input$livestock_id & State == input$state_id, year >= input$yearRange[1], year <= input$yearRange[2]) %>%
    group_by(year) %>% 
    aggregate(Count ~ year, FUN = sum) %>% as_tsibble(index = year)
    
    # Check if there are enough data points to plot a line
    if (nrow(filtered_df) < 2) {
      return(NULL)  # No plot if less than 2 data points
    }
    # specify the range of time for data to train foecast model.
    train <- filtered_df |>
      filter_index(as.character(input$trainRange[1]) ~ as.character(input$trainRange[2]))
    # Train the forecast model.
    fit <- train %>% model(
      Mean = MEAN(Count),
    )
    
    livestock_fc <- fit |> forecast(h = input$forecasts_range)
    
    # Not in use but useful.
    if (TRUE) {
      
    ggplot(data = filtered_df, aes(x = year, y = Count, group = 1)) +
      geom_line(color = "blue") +
      geom_line(data = livestock_fc, aes(y = .mean), color = "red", linetype = "dashed") +
      labs(title = paste("Time Series Data for", input$livestock_id, "in", input$state_id),
           x = "Year",
           y = "Animals slaughtered") +
      theme_minimal()
    }
    
  })
}

# Run the app ---- 
shinyApp(ui = ui, server = server)





  
