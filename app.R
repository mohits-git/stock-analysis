# Load libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(quantmod)
library(PerformanceAnalytics)
library(TTR)
library(xts)
library(tidymodels)
library(modeltime)
library(timetk)
library(ranger)
library(xgboost)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Portfolio Optimization Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Portfolio Analysis", tabName = "portfolio", icon = icon("chart-line")),
      menuItem("Risk Analysis", tabName = "risk", icon = icon("warning")),
      menuItem("ML Predictions", tabName = "predictions", icon = icon("robot"))
    ),
    
    # File input
    fileInput("file", "Upload CSV File",
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    
    # Date range input
    dateRangeInput("dateRange", "Select Date Range",
                   start = "2020-01-01",
                   end = Sys.Date()),
    
    # Risk-free rate input
    numericInput("rf_rate", "Risk-free Rate (%)", 
                 value = 2.5, min = 0, max = 100, step = 0.1)
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f6f9;
        }
        .box {
          border-radius: 10px;
          box-shadow: 0 0 10px rgba(0,0,0,0.1);
        }
        .chart-container {
          background: white;
          padding: 15px;
          border-radius: 10px;
          margin-bottom: 20px;
        }
      "))
    ),
    
    tabItems(
      # Dashboard tab
      tabItem(tabName = "dashboard",
        fluidRow(
          infoBoxOutput("returnBox"),
          infoBoxOutput("volatilityBox"),
          infoBoxOutput("sharpeBox")
        ),
        fluidRow(
          box(plotlyOutput("priceTrend"), width = 8),
          box(plotlyOutput("volumeChart"), width = 4)
        ),
        fluidRow(
          box(plotlyOutput("candlestickChart"), width = 12)
        )
      ),
      
      # Portfolio Analysis tab
      tabItem(tabName = "portfolio",
        fluidRow(
          box(
            title = "Portfolio Optimization",
            plotlyOutput("efficientFrontier"),
            width = 8
          ),
          box(
            title = "Optimal Weights",
            tableOutput("optimalWeights"),
            width = 4
          )
        ),
        fluidRow(
          box(
            title = "Return Distribution",
            plotlyOutput("returnDist"),
            width = 12
          )
        )
      ),
      
      # Risk Analysis tab
      tabItem(tabName = "risk",
        fluidRow(
          box(
            title = "Value at Risk (VaR)",
            plotlyOutput("varPlot"),
            width = 6
          ),
          box(
            title = "Expected Shortfall",
            plotlyOutput("esPlot"),
            width = 6
          )
        ),
        fluidRow(
          box(
            title = "Risk Metrics",
            tableOutput("riskMetrics"),
            width = 12
          )
        )
      ),
      
      # ML Predictions tab
      tabItem(tabName = "predictions",
        fluidRow(
          box(
            title = "Model Configuration",
            width = 12,
            column(4,
              selectizeInput("selected_companies", 
                            "Choose Company to Analyze",
                            choices = NULL,
                            multiple = FALSE)
            ),
            column(4,
              selectInput("model_type",
                         "Select Model Type",
                         choices = c("Random Forest" = "rf",
                                   "XGBoost" = "xgb",
                                   "Linear Regression" = "lm"))
            ),
            column(4,
              numericInput("forecast_days",
                          "Forecast Days",
                          value = 30,
                          min = 1,
                          max = 365)
            )
          )
        ),
        fluidRow(
          box(
            title = "Feature Engineering",
            width = 12,
            column(3,
              checkboxInput("use_ma", "Use Moving Averages", TRUE)
            ),
            column(3,
              checkboxInput("use_rsi", "Use RSI", TRUE)
            ),
            column(3,
              checkboxInput("use_volatility", "Use Volatility", TRUE)
            ),
            column(3,
              actionButton("train_model", "Train Model", 
                          class = "btn-primary")
            )
          )
        ),
        fluidRow(
          box(
            title = "Model Performance",
            width = 6,
            plotlyOutput("model_performance")
          ),
          box(
            title = "Feature Importance",
            width = 6,
            plotlyOutput("feature_importance")
          )
        ),
        fluidRow(
          box(
            title = "Price Predictions",
            width = 12,
            plotlyOutput("prediction_plot")
          )
        ),
        fluidRow(
          box(
            title = "Model Metrics",
            width = 12,
            tableOutput("ml_metrics")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive data input
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    df$Date <- as.Date(df$Date)
    df <- df[df$Date >= input$dateRange[1] & df$Date <= input$dateRange[2], ]
    return(df)
  })
  
  # Calculate returns
  returns <- reactive({
    req(data())
    returns <- diff(log(data()$CloseUSD))
    returns <- na.omit(returns)
    return(returns)
  })
  
  # Dashboard outputs
  output$returnBox <- renderInfoBox({
    req(returns())
    ann_return <- mean(returns()) * 252 * 100
    infoBox(
      "Annual Return",
      paste0(round(ann_return, 2), "%"),
      icon = icon("percentage"),
      color = "green"
    )
  })
  
  output$volatilityBox <- renderInfoBox({
    req(returns())
    ann_vol <- sd(returns()) * sqrt(252) * 100
    infoBox(
      "Annual Volatility",
      paste0(round(ann_vol, 2), "%"),
      icon = icon("chart-line"),
      color = "yellow"
    )
  })
  
  output$sharpeBox <- renderInfoBox({
    req(returns())
    rf_daily <- input$rf_rate/100/252
    sharpe <- (mean(returns()) - rf_daily) / sd(returns()) * sqrt(252)
    infoBox(
      "Sharpe Ratio",
      round(sharpe, 2),
      icon = icon("balance-scale"),
      color = "blue"
    )
  })
  
  # Price trend chart
  output$priceTrend <- renderPlotly({
    req(data())
    p <- ggplot(data(), aes(x = Date, y = CloseUSD)) +
      geom_line(color = "#2c3e50") +
      theme_minimal() +
      labs(title = "Price Trend", x = "Date", y = "Price (USD)")
    ggplotly(p)
  })
  
  # Candlestick chart
  output$candlestickChart <- renderPlotly({
    req(data())
    fig <- plot_ly(data = data(), type = "candlestick",
                   x = ~Date,
                   open = ~Open,
                   high = ~High,
                   low = ~Low,
                   close = ~Close) %>%
      layout(title = "Candlestick Chart",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Price"))
    fig
  })
  
  # Volume chart
  output$volumeChart <- renderPlotly({
    req(data())
    p <- ggplot(data(), aes(x = Date, y = Volume)) +
      geom_bar(stat = "identity", fill = "#3498db") +
      theme_minimal() +
      labs(title = "Trading Volume", x = "Date", y = "Volume")
    ggplotly(p)
  })
  
  # Efficient Frontier calculation and plot
  output$efficientFrontier <- renderPlotly({
    req(returns())
    # Simplified efficient frontier calculation
    risk_free <- input$rf_rate/100
    port_returns <- seq(min(returns()), max(returns()), length.out = 100)
    port_risk <- sqrt(252) * sd(returns()) * sqrt(port_returns^2)
    
    ef_data <- data.frame(
      Return = port_returns * 252,
      Risk = port_risk
    )
    
    p <- ggplot(ef_data, aes(x = Risk, y = Return)) +
      geom_point(color = "#2ecc71") +
      geom_line() +
      theme_minimal() +
      labs(title = "Efficient Frontier",
           x = "Portfolio Risk",
           y = "Portfolio Return")
    
    ggplotly(p)
  })
  
  # Risk metrics calculation
  calculate_risk_metrics <- reactive({
    req(returns())
    VaR_95 <- quantile(returns(), 0.05)
    ES_95 <- mean(returns()[returns() <= VaR_95])
    
    data.frame(
      Metric = c("VaR (95%)", "Expected Shortfall (95%)", "Maximum Drawdown"),
      Value = c(
        round(VaR_95 * 100, 2),
        round(ES_95 * 100, 2),
        round(maxDrawdown(returns()) * 100, 2)
      )
    )
  })
  
  output$riskMetrics <- renderTable({
    calculate_risk_metrics()
  })

    # Create features for ML
  create_features <- reactive({
    req(filtered_data())
    
    data <- filtered_data() %>%
      arrange(Date) %>%
      group_by(Index) %>%
      mutate(
        # Target variable (next day's return)
        target_return = lead(CloseUSD) / CloseUSD - 1,
        
        # Price-based features
        returns = CloseUSD / lag(CloseUSD) - 1,
        log_returns = log(CloseUSD / lag(CloseUSD)),
        
        # Technical indicators
        ma10 = if(input$use_ma) rollmean(CloseUSD, k = 10, fill = NA, align = "right") else NA,
        ma20 = if(input$use_ma) rollmean(CloseUSD, k = 20, fill = NA, align = "right") else NA,
        ma50 = if(input$use_ma) rollmean(CloseUSD, k = 50, fill = NA, align = "right") else NA,
        
        # RSI
        rsi = if(input$use_rsi) RSI(CloseUSD, n = 14) else NA,
        
        # Volatility
        volatility = if(input$use_volatility) 
          rollapply(returns, width = 20, FUN = sd, fill = NA, align = "right") else NA,
        
        # Volume features
        volume_ma = rollmean(Volume, k = 10, fill = NA, align = "right"),
        volume_ratio = Volume / volume_ma
      ) %>%
      ungroup() %>%
      select(-Volume, -Open, -High, -Low, -Close, -`Adj Close`) %>%
      na.omit()
    
    return(data)
  })
  
  # Split data into training and testing
  splits <- reactive({
    req(create_features())
    
    # Use the last 20% of the data for testing
    initial_time_split(create_features(), prop = 0.8)
  })
  
  # Define and train model
  model_fit <- eventReactive(input$train_model, {
    req(splits())
    
    # Get training data
    training_data <- training(splits())
    
    # Define recipe
    recipe_spec <- recipe(target_return ~ ., data = training_data) %>%
      step_rm(Date, Index) %>%
      step_normalize(all_predictors())
    
    # Define model
    model_spec <- case_when(
      input$model_type == "rf" ~ 
        rand_forest(trees = 500) %>%
        set_engine("ranger") %>%
        set_mode("regression"),
      
      input$model_type == "xgb" ~
        boost_tree() %>%
        set_engine("xgboost") %>%
        set_mode("regression"),
      
      input$model_type == "lm" ~
        linear_reg() %>%
        set_engine("lm") %>%
        set_mode("regression")
    )
    
    # Create workflow
    workflow_fit <- workflow() %>%
      add_recipe(recipe_spec) %>%
      add_model(model_spec) %>%
      fit(training_data)
    
    return(workflow_fit)
  })
  
  # Make predictions
  predictions <- reactive({
    req(model_fit(), splits())
    
    # Get testing data
    testing_data <- testing(splits())
    
    # Make predictions
    predictions <- predict(model_fit(), testing_data) %>%
      bind_cols(testing_data) %>%
      rename(predicted_return = .pred)
    
    # Calculate predicted prices
    predictions %>%
      mutate(
        predicted_price = CloseUSD * (1 + predicted_return),
        actual_price = lead(CloseUSD)
      )
  })
  
  # Calculate future predictions
  future_predictions <- reactive({
    req(model_fit(), create_features())
    
    # Get latest data
    latest_data <- create_features() %>%
      tail(input$forecast_days)
    
    # Make future predictions
    future_preds <- predict(model_fit(), latest_data) %>%
      bind_cols(latest_data) %>%
      rename(predicted_return = .pred) %>%
      mutate(
        predicted_price = CloseUSD * (1 + predicted_return),
        prediction_date = Date + days(1)
      )
    
    return(future_preds)
  })
  
  # Plot predictions
  output$prediction_plot <- renderPlotly({
    req(predictions(), future_predictions())
    
    # Combine historical and future predictions
    plot_data <- bind_rows(
      predictions() %>% 
        select(Date, actual = CloseUSD, predicted = predicted_price),
      future_predictions() %>%
        select(Date = prediction_date, predicted = predicted_price) %>%
        mutate(actual = NA)
    )
    
    plot_ly(plot_data, x = ~Date) %>%
      add_lines(y = ~actual, name = "Actual Price",
               line = list(color = '#17BECF')) %>%
      add_lines(y = ~predicted, name = "Predicted Price",
               line = list(color = '#7F7F7F', dash = 'dash')) %>%
      layout(
        title = "Price Predictions",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Price (USD)"),
        showlegend = TRUE
      )
  })
  
  # Model metrics
  output$ml_metrics <- renderTable({
    req(predictions())
    
    # Calculate various metrics
    metrics <- predictions() %>%
      summarise(
        RMSE = sqrt(mean((predicted_price - actual_price)^2, na.rm = TRUE)),
        MAE = mean(abs(predicted_price - actual_price), na.rm = TRUE),
        MAPE = mean(abs((predicted_price - actual_price) / actual_price), na.rm = TRUE) * 100,
        R2 = cor(predicted_price, actual_price, use = "complete.obs")^2
      ) %>%
      pivot_longer(everything(), names_to = "Metric", values_to = "Value") %>%
      mutate(Value = round(Value, 4))
    
    metrics
  })
  
  # Feature importance plot (for Random Forest and XGBoost)
  output$feature_importance <- renderPlotly({
    req(model_fit())
    
    if(input$model_type %in% c("rf", "xgb")) {
      importance_scores <- model_fit() %>%
        pull_workflow_fit() %>%
        vip::vip(num_features = 10)
      
      ggplotly(importance_scores)
    }
  })
  
  # Simple ML prediction model
   observe({
    req(data())
    companies <- unique(data()$Index)
    updateSelectizeInput(session, "selected_companies",
                        choices = companies,
                        selected = companies[1])
  })
  
  # Filtered data based on selected companies
  filtered_data <- reactive({
    req(data(), input$selected_companies)
    data() %>%
      filter(Index %in% input$selected_companies)
  })
  
  # Calculate technical indicators
  technical_indicators <- reactive({
    req(filtered_data())
    
    # Create a data frame for each company
    result <- filtered_data() %>%
      group_by(Index) %>%
      do({
        df <- .
        tibble(
          Date = df$Date,
          Company = df$Index,
          Close = df$CloseUSD,
          MA20 = rollmean(df$CloseUSD, k = 20, fill = NA, align = "right"),
          MA50 = rollmean(df$CloseUSD, k = 50, fill = NA, align = "right"),
          RSI = RSI(df$CloseUSD, n = 14),
          Upper = NA,
          Lower = NA,
          Signal = NA
        )
      }) %>%
      ungroup()
    
    # Calculate Bollinger Bands (20-day SMA ± 2×SD)
    result <- result %>%
      group_by(Company) %>%
      mutate(
        SD = rollapply(Close, width = 20, FUN = sd, fill = NA, align = "right"),
        Upper = MA20 + (2 * SD),
        Lower = MA20 - (2 * SD),
        # Generate trading signals based on MA crossover
        Signal = ifelse(MA20 > MA50, Close * 1.001, Close * 0.999)
      ) %>%
      ungroup()
    
    return(result)
  })
  
  # Update prediction plot
  output$predictionPlot <- renderPlotly({
    req(technical_indicators())
    
    plot_data <- technical_indicators()
    
    p <- plot_ly(plot_data, x = ~Date) %>%
      add_lines(y = ~Close, name = "Actual Price", 
                line = list(color = '#17BECF')) %>%
      add_lines(y = ~Signal, name = "Predicted", 
                line = list(color = '#7F7F7F', dash = 'dash')) %>%
      layout(
        title = "Price Prediction with MA Crossover Strategy",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Price (USD)"),
        showlegend = TRUE
      )
    
    if(length(input$selected_companies) > 1) {
      p <- p %>% group_by(Company)
    }
    
    p
  })
  
  # Add technical indicators plot
  output$technicalIndicators <- renderPlotly({
    req(technical_indicators())
    
    plot_data <- technical_indicators()
    
    p <- plot_ly(plot_data, x = ~Date) %>%
      add_lines(y = ~Close, name = "Price", 
                line = list(color = '#17BECF')) %>%
      add_lines(y = ~MA20, name = "20-day MA", 
                line = list(color = '#7F7F7F')) %>%
      add_lines(y = ~MA50, name = "50-day MA", 
                line = list(color = '#DE3163')) %>%
      add_lines(y = ~Upper, name = "Upper BB", 
                line = list(color = '#90EE90', dash = 'dash')) %>%
      add_lines(y = ~Lower, name = "Lower BB", 
                line = list(color = '#90EE90', dash = 'dash'))
    
    if(length(input$selected_companies) > 1) {
      p <- p %>% group_by(Company)
    }
    
    p %>% layout(
      title = "Technical Indicators",
      xaxis = list(title = "Date"),
      yaxis = list(title = "Price (USD)"),
      showlegend = TRUE
    )
  })
  
  # Add model metrics output
  output$modelMetrics <- renderTable({
    req(technical_indicators())
    
    # Calculate basic prediction metrics
    metrics <- technical_indicators() %>%
      group_by(Company) %>%
      summarise(
        Accuracy = mean(abs(Signal - Close) / Close, na.rm = TRUE),
        `MA20 Trend` = tail(MA20, 1) > tail(MA20, 2)[1],
        `RSI` = tail(RSI, 1),
        .groups = 'drop'
      ) %>%
      mutate(
        Accuracy = scales::percent(1 - Accuracy),
        `MA20 Trend` = ifelse(`MA20 Trend`, "Upward", "Downward"),
        `RSI` = round(RSI, 2)
      )
    
    metrics
  })
}

# Run the application
shinyApp(ui = ui, server = server)
