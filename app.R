# R Shinny application
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(treemap)
library(dplyr)
library(plotly)
library(heatmaply)
library(caret)
library(tseries)

# User Interface
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Enhanced Financial Portfolio Optimization and Risk Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("company", "Select a Company:", 
                  choices = unique(df$Company), multiple = TRUE),
      sliderInput("investment", "Investment Amount (in INR):", 
                  min = 1000, max = 100000, value = 10000, step = 1000),
      actionButton("optimize", "Optimize Portfolio"),
      actionButton("analyzeRisk", "Analyze Risk"),
      dateRangeInput("dateRange", "Select Date Range:", 
                     start = min(df$Date), end = max(df$Date))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", tableOutput("summaryTable")),
        tabPanel("Time Series Analysis", plotlyOutput("lineChart")),
        tabPanel("Correlation Analysis", plotlyOutput("heatmap")),
        tabPanel("Treemap Analysis", plotlyOutput("treemap")),
        tabPanel("Risk Analysis", verbatimTextOutput("riskMetrics")),
        tabPanel("Optimization Results", tableOutput("optResults"))
      )
    )
  )
)


# Server Logic
server <- function(input, output) {
  # Load and preprocess data
  df <- read.csv("~/Desktop/r-lab/stock.csv")
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  
  # Filter data based on user selection
  filtered_data <- reactive({
    df %>% filter(Company %in% input$company, Date >= input$dateRange[1], Date <= input$dateRange[2])
  })
  
  # Generate summary statistics
  output$summaryTable <- renderTable({
    selected_data <- filtered_data()
    summary_stats <- selected_data %>% 
      group_by(Company) %>% 
      summarize(Mean_Close = mean(Close, na.rm = TRUE),
                SD_Close = sd(Close, na.rm = TRUE),
                Max_Profit = max(Company_Profit, na.rm = TRUE))
    return(summary_stats)
  })
  
  # Time series analysis for selected companies
  output$lineChart <- renderPlotly({
    selected_data <- filtered_data()
    plot_ly(selected_data, x = ~Date, y = ~Close, color = ~Company, type = 'scatter', mode = 'lines') %>%
      layout(title = "Closing Prices Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Closing Price"))
  })
  
  # Correlation heatmap
  output$heatmap <- renderPlotly({
    selected_data <- filtered_data()
    correlation_matrix <- cor(selected_data %>% select(Open, Close, Low, High, Company_Profit, Company_Revenue))
    heatmaply::heatmaply(correlation_matrix, colors = RColorBrewer::brewer.pal(9, "Blues"),
                         xlab = "Features", ylab = "Features", main = "Correlation Heatmap")
  })
  
  # Treemap visualization
  output$treemap <- renderPlotly({
    selected_data <- filtered_data()
    treemap::treemap(selected_data, 
                     index = "Company", 
                     vSize = "Company_Profit", 
                     vColor = "Company_Revenue", 
                     type = "value",
                     title = "Profit and Revenue Treemap",
                     palette = "RdYlBu")
  })
  
  # Risk analysis
  output$riskMetrics <- renderPrint({
    selected_data <- filtered_data()
    VaR <- quantile(selected_data$Close, 0.05, na.rm = TRUE)  # Example for 5% VaR
    CVaR <- mean(selected_data$Close[selected_data$Close <= VaR], na.rm = TRUE)
    
    cat("Value at Risk (VaR 5%):", round(VaR, 2), "\n")
    cat("Conditional Value at Risk (CVaR):", round(CVaR, 2), "\n")
  })
  
  # Portfolio optimization
  output$optResults <- renderTable({
    selected_data <- filtered_data()
    returns <- selected_data %>% select(Open, Close)
    mean_returns <- colMeans(returns, na.rm = TRUE)
    cov_matrix <- cov(returns, use = "complete.obs")
    
    # Markowitz Portfolio Optimization logic
    portfolio_opt <- portfolio.optim(mean_returns, cov_matrix, pm = mean(mean_returns))
    
    data.frame(
      Weights = portfolio_opt$pw,
      Expected_Return = round(portfolio_opt$pm, 2),
      Portfolio_Risk = round(portfolio_opt$ps, 2)
    )
  })
}

# Combine UI and server to create the app
shinyApp(ui = ui, server = server)