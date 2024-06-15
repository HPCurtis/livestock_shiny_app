library(fpp3)
library(shiny)

setwd("/home/harrison/Desktop/gitHubRepos/livestock_shiny_app/")

if (!file.exists("data/aus_livestock")) {
  path <- "/home/harrison/Desktop/gitHubRepos/livestock_shiny_app/data/aus_livestock"
  write.csv(aus_livestock, path, row.names = FALSE)
  print("Austalian livestock data written to repository")
  # read in data file
  df <- read.csv("data/aus_livestock")
 
} else {
  print("File already exists")
  # Read in data file if already exists in repository
  df <- read.csv("data/aus_livestock")
}

# Ensure the data types are correct
df$Month <- as.Date(df$Month)
df$year <- as.integer(substr(df$Month, 1, 4))

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
 titlePanel("Australian Livestock App"),
)


# Define UI for the app
ui <- fluidPage(
  # App title
  titlePanel("Australian Livestock App"),
  
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
                  sep = "")
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
    aggregate(Count ~ year, FUN = sum)
    
    # Check if there are enough data points to plot a line
    if (nrow(filtered_df) < 2) {
      return(NULL)  # No plot if less than 2 data points
    }
                                    
    ggplot(data = filtered_df, aes(x = year, y = Count, group = 1)) +
      geom_line(color = "blue") +
      labs(title = paste("Time Series Data for", input$livestock_id, "in", input$state_id),
           x = "Date",
           y = "Count") +
      theme_minimal()
  })
}


# Run the app ---- 
shinyApp(ui = ui, server = server)





  
