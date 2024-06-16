# Import pre-requiste packages for data and forecasts.
library(fpp3)
library(shiny)

# Import Australian livestock data.
df<- aus_livestock

# Add year column are correct
df$year <- as.integer(substr(df$Month, 1, 4))

# Define UI for the app
ui <- fluidPage(
  # App title
  titlePanel("Australian Livestock forecasts"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "livestock_id", label = "Livestock", choices = unique(df$Animal)),
      selectInput(inputId = "state_id", label = "State", choices = unique(df$State)),
      sliderInput("yearRange",
                  label = "Select Year Range",
                  min = min(df$year),
                  max = max(df$year),
                  value = c(min(df$year), max(df$year)),
                  step = 1,
                  sep = ""),
      
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
    
    mainPanel(
      plotOutput("timeseries_plot")
    )
  )
)

# Define server logic
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
    
    train <- filtered_df |>
      filter_index(as.character(input$trainRange[1]) ~ as.character(input$trainRange[2]))
    
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





  
