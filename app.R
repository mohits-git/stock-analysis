# R Shinny application
install.packages(c("shiny", "shinyWidgets", "shinythemes", "treemap", "dplyr", "plotly", "heatmaply", "caret"))
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(treemap)
library(dplyr)
library(plotly)
library(heatmaply)
library(caret)

# User Interface
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Financial Portfolio Optimization and Risk Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("company", "Select a Company:", 
                  choices = unique(df$Company)),
      sliderInput("investment", "Investment Amount (in INR):", 
                  min = 1000, max = 100000, value = 10000, step = 1000),
      actionButton("optimize", "Optimize Portfolio")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", tableOutput("summaryTable")),
        tabPanel("Visualizations",
                 plotlyOutput("lineChart"),
                 plotlyOutput("treemap"),
                 plotlyOutput("heatmap")),
        tabPanel("Model Predictions", verbatimTextOutput("modelSummary"))
      )
    )
  )
)

# Server Logic
server <- function(input, output) {
  # Load and preprocess data
  df <- read.csv("~/Desktop/r-lab/stock.csv")
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  
  # Create summary statistics
  output$summaryTable <- renderTable({
    selected_company <- df %>% filter(Company == input$company)
    summary_stats <- selected_company %>% 
      summarize(Mean_Close = mean(Close, na.rm = TRUE),
                SD_Close = sd(Close, na.rm = TRUE),
                Max_Profit = max(Company_Profit, na.rm = TRUE))
    return(summary_stats)
  })
  
  # Line chart for selected company
  output$lineChart <- renderPlotly({
    selected_company <- df %>% filter(Company == input$company)
    plot_ly(selected_company, x = ~Date, y = ~Close, type = 'scatter', mode = 'lines',
            line = list(color = 'blue')) %>%
      layout(title = paste("Closing Prices of", input$company), 
             xaxis = list(title = "Date"), 
             yaxis = list(title = "Closing Price"))
  })
  
  # Treemap visualization
  output$treemap <- renderPlotly({
    treemap_data <- df %>% 
      group_by(Company) %>% 
      summarize(Total_Profit = sum(Company_Profit, na.rm = TRUE),
                Total_Revenue = sum(Company_Revenue, na.rm = TRUE))
    
    plot_ly(
      type = "treemap",
      labels = treemap_data$Company,
      parents = "",
      values = treemap_data$Total_Profit,
      textinfo = "label+value+percent entry",
      marker = list(colors = ~treemap_data$Total_Revenue, colorscale = "Blues")
    ) %>%
      layout(title = "Treemap of Company Profits and Revenues")
  })
  
  # Heatmap visualization of correlation matrix
  output$heatmap <- renderPlotly({
    numeric_df <- df %>% select(Open, Close, Low, High, Company_Profit, Company_Revenue)
    correlation_matrix <- cor(numeric_df, use = "complete.obs")
    heatmaply(correlation_matrix, xlab = "Metrics", ylab = "Metrics",
              main = "Correlation Matrix of Financial Metrics")
  })
  
  # Simple predictive model
  output$modelSummary <- renderPrint({
    train_data <- df %>% select(Open, Close, Low, High, Company_Profit, Company_Revenue)
    train_data <- na.omit(train_data)
    
    set.seed(123)
    model <- train(Close ~ Open + Low + High + Company_Profit + Company_Revenue, 
                   data = train_data, 
                   method = "lm")
    summary(model)
  })
}

# Combine UI and server to create the app
shinyApp(ui = ui, server = server)