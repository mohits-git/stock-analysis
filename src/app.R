# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(plotly)
library(zoo)
library(caret)  # For prediction model
library(randomForest)  # For more advanced predictions
library(conflicted)

# Declare preferences for conflicting functions
conflicts_prefer(
  dplyr::filter,
  dplyr::lag,
  dplyr::intersect,
  dplyr::setdiff,
  dplyr::setequal,
  dplyr::union,
  shinydashboard::box,
  plotly::layout,
  shiny::observe  # Add this line to prefer shiny::observe
)

# Increase file upload limit to 30 MB if necessary
options(shiny.maxRequestSize = 30 * 1024^2)

# UI Definition with dashboard style
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Stock Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload & Select", tabName = "upload", icon = icon("upload")),
      fileInput("file", "Upload CSV file with stock data", accept = c(".csv"), multiple = FALSE),
      selectInput("stock", "Stock Symbol:", choices = NULL, selected = NULL),
      dateRangeInput("date_range", "Date Range:", start = NULL, end = NULL),
      sliderInput("short_ma_days", "Short Moving Average Days:", min = 1, max = 30, value = 5),
      sliderInput("long_ma_days", "Long Moving Average Days:", min = 1, max = 60, value = 20)
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .box { border-radius: 10px; }
      .box-title { font-size: 20px; font-weight: bold; color: #fff; }
      .skin-blue .main-header .logo { background-color: #2c3e50; }
      .skin-blue .main-header .navbar { background-color: #1abc9c; }
      .skin-blue .main-sidebar { background-color: #34495e; }
      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a { background-color: #1abc9c; }
      .skin-blue .main-sidebar .sidebar .sidebar-menu a { color: #ecf0f1; }
      .box.box-solid.box-primary>.box-header { color: #fff; background: #1abc9c; }
      .box.box-solid.box-primary { border: 1px solid #1abc9c; }
    "))),
    fluidRow(
      shinydashboard::box(title = "Average Closing Price", width = 6, status = "info", solidHeader = TRUE, plotlyOutput("avgCloseChart")),
      shinydashboard::box(title = "Total Volume", width = 6, status = "warning", solidHeader = TRUE, plotlyOutput("totalVolumeChart")),
      shinydashboard::box(title = "Average High and Low Prices", width = 12, status = "success", solidHeader = TRUE, plotlyOutput("avgHighLowChart")),
      shinydashboard::box(title = "Stock Price Over Time", width = 12, status = "primary", solidHeader = TRUE, plotlyOutput("priceChart")),
      shinydashboard::box(title = "Volume Over Time", width = 12, status = "warning", solidHeader = TRUE, plotlyOutput("volumeChart")),
      shinydashboard::box(title = "Moving Average", width = 12, status = "info", solidHeader = TRUE, plotlyOutput("movingAverageChart")),
      shinydashboard::box(title = "Candlestick Chart", width = 12, status = "danger", solidHeader = TRUE, plotlyOutput("candlestickChart")),
      shinydashboard::box(title = "Next Day Opening Price Prediction", width = 12, status = "info", solidHeader = TRUE, verbatimTextOutput("nextDayPrediction")),
      shinydashboard::box(title = "Automated Commentary", width = 12, status = "success", solidHeader = TRUE, verbatimTextOutput("commentary")),
      shinydashboard::box(title = "Risk Analysis", width = 12, status = "success", solidHeader = TRUE, verbatimTextOutput("riskAnalysis")),
      
      # Move Dataset Summary here
      shinydashboard::box(title = "Dataset Summary", width = 12, status = "primary", solidHeader = TRUE, verbatimTextOutput("dataSummary"))
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Load data from user-uploaded file
  stock_data <- reactive({
    if (!is.null(input$file)) {
      data <- read.csv(input$file$datapath)
      data$Date <- as.Date(data$Date)  # Ensure Date column is in Date format
      required_cols <- c("Index", "Date", "Open", "High", "Low", "Close", "AdjClose", "Volume", "CloseUSD")
      missing_cols <- setdiff(required_cols, colnames(data))
      
      if (length(missing_cols) == 0) {
        return(data)
      } else {
        showNotification(
          paste("Uploaded file is missing required columns:", paste(missing_cols, collapse = ", ")),
          type = "error"
        )
        return(NULL)
      }
    } else {
      return(NULL)
    }
  })
  
  # Filtered data based on user input
  filtered_data <- reactive({
    req(stock_data(), input$stock, input$date_range)
    stock_data() %>%
      dplyr::filter(Index == input$stock, Date >= input$date_range[1], Date <= input$date_range[2])
  })
  
  output$dataSummary <- renderPrint({
    req(stock_data())
    summary(stock_data())
  })
  
  observe({
    data <- stock_data()
    if (!is.null(data)) {
      updateSelectInput(session, "stock", choices = unique(data$Index), selected = unique(data$Index)[1])
      updateDateRangeInput(session, "date_range", start = min(data$Date), end = max(data$Date))
    }
  })
  
  output$avgCloseChart <- renderPlotly({
    req(filtered_data())
    avg_close <- filtered_data() %>%
      group_by(Date) %>%
      summarize(Average_Close = mean(Close, na.rm = TRUE))
    
    plot_ly(avg_close, x = ~Date, y = ~Average_Close, type = 'scatter', mode = 'lines', line = list(color = 'blue')) %>%
      plotly::layout(title = paste("Average Closing Price for", input$stock))
  })
  
  output$totalVolumeChart <- renderPlotly({
    req(filtered_data())
    total_volume <- filtered_data() %>%
      group_by(Date) %>%
      summarize(Total_Volume = sum(Volume, na.rm = TRUE))
    
    plot_ly(total_volume, x = ~Date, y = ~Total_Volume, type = 'scatter', mode = 'lines', line = list(color = 'red')) %>%
      plotly::layout(title = paste("Total Volume for", input$stock))
  })
  
  output$avgHighLowChart <- renderPlotly({
    req(filtered_data())
    avg_high_low <- filtered_data() %>%
      group_by(Date) %>%
      summarize(Average_High = mean(High, na.rm = TRUE), Average_Low = mean(Low, na.rm = TRUE))
    
    plot_ly(avg_high_low, x = ~Date, y = ~Average_High, type = 'scatter', mode = 'lines', line = list(color = 'green'), name = "High") %>%
      add_trace(x = ~Date, y = ~Average_Low, type = 'scatter', mode = 'lines', line = list(color = 'orange'), name = "Low") %>%
      plotly::layout(title = paste("Average High and Low Prices for", input$stock), legend = list(orientation = "h"))
  })
  
  output$priceChart <- renderPlotly({
    req(filtered_data())
    price_data <- filtered_data() %>%
      select(Date, Open, High, Low, Close)
    
    plot_ly(price_data, x = ~Date, y = ~Open, type = 'scatter', mode = 'lines', line = list(color = 'blue'), name = "Open") %>%
      add_trace(x = ~Date, y = ~High, type = 'scatter', mode = 'lines', line = list(color = 'green'), name = "High") %>%
      add_trace(x = ~Date, y = ~Low, type = 'scatter', mode = 'lines', line = list(color = 'orange'), name = "Low") %>%
      add_trace(x = ~Date, y = ~Close, type = 'scatter', mode = 'lines', line = list(color = 'red'), name = "Close") %>%
      plotly::layout(title = paste("Stock Price Over Time for", input$stock), legend = list(orientation = "h"))
  })
  
  output$volumeChart <- renderPlotly({
    req(filtered_data())
    volume_data <- filtered_data() %>%
      select(Date, Volume)
    
    plot_ly(volume_data, x = ~Date, y = ~Volume, type = 'scatter', mode = 'lines', line = list(color = 'purple')) %>%
      plotly::layout(title = paste("Volume Over Time for", input$stock))
  })
  
  output$movingAverageChart <- renderPlotly({
    req(filtered_data())
    moving_avg <- filtered_data() %>%
      mutate(
        Short_MA = zoo::rollmean(Close, input$short_ma_days, fill = NA),
        Long_MA = zoo::rollmean(Close, input$long_ma_days, fill = NA)
      )
    
    plot_ly(moving_avg, x = ~Date, y = ~Short_MA, type = 'scatter', mode = 'lines', line = list(color = 'blue'), name = paste(input$short_ma_days, "Day MA")) %>%
      add_trace(x = ~Date, y = ~Long_MA, type = 'scatter', mode = 'lines', line = list(color = 'red'), name = paste(input$long_ma_days, "Day MA")) %>%
      plotly::layout(title = paste("Moving Averages for", input$stock))
  })
  
  output$candlestickChart <- renderPlotly({
    req(filtered_data())
    plot_ly(data = filtered_data(), x = ~Date, type = "candlestick",
            open = ~Open, high = ~High, low = ~Low, close = ~Close) %>%
      plotly::layout(title = paste("Candlestick Chart for", input$stock))
  })
  
  output$nextDayPrediction <- renderPrint({
    req(filtered_data())
    data <- filtered_data()
    
    if (nrow(data) > 1) {
      model <- randomForest(Close ~ Open + High + Low + Volume, data = data)
      next_day_data <- data[nrow(data), ]
      prediction <- predict(model, newdata = next_day_data)
      paste("Predicted opening price for the next day:", round(prediction, 2))
    } else {
      "Not enough data for prediction"
    }
  })
  
  output$commentary <- renderPrint({
    req(filtered_data())
    data <- filtered_data()
    
    if (nrow(data) > 1) {
      latest_data <- data[nrow(data), ]
      commentary <- paste("As of", latest_data$Date, "the stock closed at", round(latest_data$Close, 2),
                          "with a volume of", latest_data$Volume, ". The high was", round(latest_data$High, 2),
                          "and the low was", round(latest_data$Low, 2), ".")
      commentary
    } else {
      "Not enough data for commentary"
    }
  })
  
  output$riskAnalysis <- renderPrint({
    req(filtered_data())
    data <- filtered_data()
    
    if (nrow(data) > 1) {
      risk <- sd(data$Close, na.rm = TRUE)
      paste("Risk (standard deviation of closing prices):", round(risk, 2))
    } else {
      "Not enough data for risk analysis"
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
