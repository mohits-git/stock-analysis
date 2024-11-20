# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(plotly)
library(zoo)
library(caret)
library(randomForest)
library(conflicted)
library(DBI)
library(RSQLite)
library(sodium) # for password hashing
library(shinyjs)
library(googleAuthR)
library(httr)
library(jsonlite)

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
  shiny::observe,
  httr::config
)

# ENVs
readRenviron("../.env")

google_client_id <- Sys.getenv("GOOGLE_CLIENT_ID")
google_client_secret <- Sys.getenv("GOOGLE_CLIENT_SECRET")
google_redirect_uri <- Sys.getenv("GOOGLE_REDIRECT_URI")

# Initialize SQLite database for users
init_db <- function() {
  con <- dbConnect(RSQLite::SQLite(), "users.db")
  
  # Create users table if it doesn't exist
  if (!dbExistsTable(con, "users")) {
    dbExecute(con, "CREATE TABLE users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE,
      password TEXT,
      email TEXT UNIQUE NOT NULL,
      google_id TEXT UNIQUE,
      google_email TEXT,
      profile_picture TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )")
  }
  
  dbDisconnect(con)
}

# Initialize database
init_db()

# Authentication functions
hash_password <- function(password) {
  password_hash <- sodium::password_store(password)
  return(password_hash)
}

verify_password <- function(password, hash) {
  sodium::password_verify(hash, password)
}

# Create Login Page UI
loginPageUI <- function() {
  div(class = "auth-container",
    tags$head(
      tags$style(HTML("
        .auth-container {
          height: 100vh;
          background: linear-gradient(135deg, #1abc9c, #2c3e50);
          display: flex;
          justify-content: center;
          align-items: center;
        }
        .auth-box {
          background: white;
          padding: 30px;
          border-radius: 10px;
          box-shadow: 0 5px 15px rgba(0,0,0,0.2);
          width: 100%;
          max-width: 400px;
        }
        .auth-title {
          text-align: center;
          color: #2c3e50;
          margin-bottom: 30px;
        }
        .auth-input {
          margin-bottom: 15px;
        }
        .auth-btn {
          width: 100%;
          padding: 10px;
          border: none;
          border-radius: 5px;
          color: white;
          font-size: 16px;
          cursor: pointer;
          transition: background-color 0.3s;
        }
        .login-btn {
          background-color: #1abc9c;
        }
        .login-btn:hover {
          background-color: #16a085;
        }
        .signup-btn {
          background-color: #3498db;
          margin-top: 10px;
        }
        .signup-btn:hover {
          background-color: #2980b9;
        }
        .auth-message {
          text-align: center;
          margin-top: 15px;
          color: #e74c3c;
        }
        .auth-switch {
          text-align: center;
          margin-top: 20px;
          color: #7f8c8d;
        }
        .auth-switch a {
          color: #3498db;
          text-decoration: none;
        }
        .auth-switch a:hover {
          text-decoration: underline;
        }
      "))
    ),
    div(class = "auth-box",
      useShinyjs(),
      h2(class = "auth-title", "Login"),
      div(class = "auth-input",
        textInput("login_username", "Username", placeholder = "Enter your username")
      ),
      div(class = "auth-input",
        passwordInput("login_password", "Password", placeholder = "Enter your password")
      ),
      div(class = "auth-message", textOutput("login_message")),
      actionButton("login_btn", "Login", class = "auth-btn login-btn"),
      tags$hr(style = "margin: 20px 0;"),
      div(class = "auth-switch",
        "Or sign in with:",
        actionButton("google_signin", "Sign in with Google",
          class = "auth-btn",
          style = "background-color: #4285f4; margin-top: 10px;",
          icon = icon("google")
        )
      ),
      div(class = "auth-switch",
        "Don't have an account?",
        actionLink("go_to_signup", "Sign up")
      )
    )
  )
}

# Create Signup Page UI
signupPageUI <- function() {
  div(class = "auth-container",
    div(class = "auth-box",
      useShinyjs(),
      h2(class = "auth-title", "Sign Up"),
      div(class = "auth-input",
        textInput("signup_username", "Username", placeholder = "Choose a username")
      ),
      div(class = "auth-input",
        textInput("signup_email", "Email", placeholder = "Enter your email")
      ),
      div(class = "auth-input",
        passwordInput("signup_password", "Password", placeholder = "Choose a password")
      ),
      div(class = "auth-input",
        passwordInput("signup_confirm_password", "Confirm Password", placeholder = "Confirm your password")
      ),
      div(class = "auth-message", textOutput("signup_message")),
      actionButton("signup_btn", "Sign Up", class = "auth-btn signup-btn"),
      div(class = "auth-switch",
        "Already have an account?",
        actionLink("go_to_login", "Login")
      )
    )
  )
}

# Increase file upload limit to 30 MB
options(shiny.maxRequestSize = 30 * 1024^2)

# Create Landing Page UI
landingPageUI <- function() {
  tagList(
    tags$head(
      tags$style(HTML("
        body {
          margin: 0;
          font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
        }
        .landing-container {
          min-height: 100vh;
          background: linear-gradient(135deg, #1abc9c, #2c3e50);
          color: white;
        }
        .landing-content {
          padding: 50px 20px;
          max-width: 1200px;
          margin: 0 auto;
        }
        .hero-section {
          text-align: center;
          padding: 60px 0;
        }
        .feature-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
          gap: 30px;
          margin: 50px 0;
        }
        .feature-box {
          background: rgba(255, 255, 255, 0.1);
          padding: 25px;
          border-radius: 10px;
          text-align: center;
          backdrop-filter: blur(10px);
          transition: transform 0.3s;
        }
        .feature-box:hover {
          transform: translateY(-5px);
        }
        .feature-icon {
          font-size: 40px;
          margin-bottom: 15px;
        }
        .get-started-btn {
          background-color: #e74c3c;
          color: white;
          padding: 15px 40px;
          border: none;
          border-radius: 25px;
          font-size: 18px;
          cursor: pointer;
          transition: background-color 0.3s;
          margin-top: 30px;
        }
        .get-started-btn:hover {
          background-color: #c0392b;
        }
        .section {
          margin: 40px 0;
        }
      "))
    ),
    div(class = "landing-container",
      div(class = "landing-content",
        # Hero Section
        div(class = "hero-section",
          h1("Stock Analysis Dashboard", style = "font-size: 3.5em; margin-bottom: 20px;"),
          p("Your comprehensive solution for stock market analysis and predictions", 
            style = "font-size: 1.4em; margin-bottom: 40px;"),
          actionButton("getStarted", "Get Started", class = "get-started-btn"),
          downloadButton("downloadTemplate", "Download Template", 
            class = "get-started-btn", 
            style = "margin-left: 15px; background-color: #3498db;")
        ),
        
        # Features Grid
        div(class = "feature-grid",
          div(class = "feature-box",
            div(class = "feature-icon", icon("chart-line")),
            h3("Advanced Analytics"),
            p("Interactive charts and comprehensive technical analysis tools")
          ),
          div(class = "feature-box",
            div(class = "feature-icon", icon("robot")),
            h3("AI Predictions"),
            p("Machine learning powered price predictions")
          ),
          div(class = "feature-box",
            div(class = "feature-icon", icon("gauge-high")),
            h3("Real-time Monitoring"),
            p("Track performance with real-time updates")
          )
        ),
        
        # How It Works Section
        div(class = "section",
          h2("How It Works", style = "text-align: center; margin-bottom: 30px;"),
          div(class = "feature-grid",
            div(class = "feature-box",
              h4("1. Upload Data"),
              p("Import your stock data through CSV files")
            ),
            div(class = "feature-box",
              h4("2. Analyze"),
              p("Use powerful tools to analyze market trends")
            ),
            div(class = "feature-box",
              h4("3. Predict"),
              p("Get AI-powered insights for informed decisions")
            )
          )
        )
      )
    )
  )
}

# Create Main Dashboard UI (your existing dashboard)
dashboardUI <- function() {
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Stock Analysis Dashboard"),
    dashboardSidebar(
      tags$style(HTML("
        /* Sidebar Styling */
        .main-sidebar {
          background-color: #34495e !important;
          color: #ecf0f1 !important;
          padding-top: 60px;
        }
        .sidebar-menu li a {
          color: #ecf0f1 !important;
          background-color: transparent !important;
        }
        .sidebar-menu li a:hover {
          background-color: #2c3e50 !important;
        }
        .sidebar-menu li.active a {
          background-color: #1abc9c !important;
        }
        /* Header Styling */
        .main-header .logo {
          background-color: #2c3e50 !important;
          color: #ecf0f1 !important;
        }
        .main-header .navbar {
          background-color: #1abc9c !important;
        }
        /* Input Styling in Sidebar */
        .sidebar .shiny-input-container {
          padding: 12px 15px;
        }
        .sidebar .shiny-input-container label {
          color: #ecf0f1 !important;
        }
        .sidebar .irs-grid-text {
          color: #ecf0f1 !important;
        }
        .sidebar .irs-min, .sidebar .irs-max, .sidebar .irs-single, .sidebar .irs-from, .sidebar .irs-to {
          background: #2c3e50 !important;
          color: #ecf0f1 !important;
        }
        .sidebar .irs-bar {
          background: #1abc9c !important;
        }
        .sidebar .irs-bar-edge {
          background: #1abc9c !important;
        }
        .sidebar .irs-line {
          background: #ecf0f1 !important;
        }
        /* File input styling */
        .sidebar .btn-file {
          background-color: #1abc9c !important;
          color: #ecf0f1 !important;
          border: none !important;
        }
        .sidebar .btn-file:hover {
          background-color: #16a085 !important;
        }
      ")),
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line")),
        fileInput("file", "Upload CSV file with stock data", accept = c(".csv"), multiple = FALSE),
        downloadButton("downloadTemplate", "Download Template", 
            style = "margin: 15px; width: 90%; background-color: #1abc9c; border: none;"),
        selectInput("stock", "Stock Symbol:", choices = NULL, selected = NULL),
        dateRangeInput("date_range", "Date Range:", start = NULL, end = NULL),
        sliderInput("short_ma_days", "Short Moving Average Days:", min = 1, max = 30, value = 5),
        sliderInput("long_ma_days", "Long Moving Average Days:", min = 1, max = 60, value = 20)
      )
    ),
    dashboardBody(
      tags$style(HTML("
        /* Box Styling */
        .box { 
          border-radius: 10px;
          border: none;
          box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        }
        .box-title { 
          font-size: 20px;
          font-weight: bold;
          color: #fff;
        }
        /* Box Headers */
        .box.box-info > .box-header {
          background-color: #3498db !important;
        }
        .box.box-warning > .box-header {
          background-color: #f1c40f !important;
        }
        .box.box-success > .box-header {
          background-color: #2ecc71 !important;
        }
        .box.box-danger > .box-header {
          background-color: #e74c3c !important;
        }
        .box.box-primary > .box-header {
          background-color: #1abc9c !important;
        }
        /* Content area styling */
        .content-wrapper {
          background-color: #f5f6fa !important;
        }
      ")),
      
      # Your existing dashboard content
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
        shinydashboard::box(title = "Dataset Summary", width = 12, status = "primary", solidHeader = TRUE, verbatimTextOutput("dataSummary"))
      )
    )
  )
}

# Combined UI with conditional rendering
ui <- function() {
  fluidPage(
    useShinyjs(),
    tags$head(
      tags$script("
        Shiny.addCustomMessageHandler('redirectToGoogle', function(authUrl) {
          window.location.href = authUrl;
        });
      ")
    ),
    uiOutput("currentPage")
  )
}

## Google OAuth
# Add these Google authentication functions
setup_google_auth <- function() {
  options(googleAuthR.scopes.selected = c(
    "https://www.googleapis.com/auth/userinfo.email",
    "https://www.googleapis.com/auth/userinfo.profile"
  ))
  
  # Replace with your OAuth credentials
  options(googleAuthR.webapp.client_id =google_client_id,
          googleAuthR.webapp.client_secret =google_client_secret,
          googleAuthR.webapp.redirect_uri =google_redirect_uri)
  # Configure for same window authentication
  # options(googleAuthR.webapp.use_basic_auth = TRUE,
  #         googleAuthR.webapp.redirect_on_signout = TRUE,
  #         googleAuthR.webapp.popup = FALSE)
}

# Modify the handle_google_auth function to handle the OAuth code
handle_google_auth <- function(code, session) {
  # Exchange the authorization code for an access token
  token_response <- POST(
    "https://oauth2.googleapis.com/token",
    body = list(
      code = code,
      client_id = google_client_id,
      client_secret = google_client_secret,
      redirect_uri = "http://localhost:6740",
      grant_type = "authorization_code"
    ),
    encode = "form"
  )
  
  if (status_code(token_response) == 200) {
    token_data <- fromJSON(rawToChar(token_response$content))
    access_token <- token_data$access_token
    
    # Get user info using the access token
    user_info_response <- GET(
      "https://www.googleapis.com/oauth2/v1/userinfo",
      add_headers(Authorization = paste("Bearer", access_token))
    )
    
    if (status_code(user_info_response) == 200) {
      user_info <- fromJSON(rawToChar(user_info_response$content))
      
      # Connect to database
      con <- dbConnect(RSQLite::SQLite(), "users.db")
      
      # Check if user exists
      existing_user <- dbGetQuery(con, sprintf(
        "SELECT * FROM users WHERE google_id = '%s' OR email = '%s'",
        user_info$id, user_info$email
      ))
      
      if (nrow(existing_user) == 0) {
        # Create new user
        dbExecute(con, 
          "INSERT INTO users (username, email, google_id, google_email, profile_picture) 
           VALUES (?, ?, ?, ?, ?)",
          params = list(
            user_info$email,
            user_info$email,
            user_info$id,
            user_info$email,
            user_info$picture
          )
        )
        
        # Get the newly created user
        user <- dbGetQuery(con, sprintf(
          "SELECT * FROM users WHERE google_id = '%s'",
          user_info$id
        ))
      } else {
        # Update existing user's Google info
        dbExecute(con,
          "UPDATE users SET 
           google_id = ?, 
           google_email = ?, 
           profile_picture = ? 
           WHERE email = ?",
          params = list(
            user_info$id,
            user_info$email,
            user_info$picture,
            user_info$email
          )
        )
        user <- existing_user
      }
      
      dbDisconnect(con)
      
      return(list(
        username = user_info$email,
        email = user_info$email,
        profile_picture = user_info$picture
      ))
    }
  }
  return(NULL)
}

get_google_user_info <- function(access_token) {
  # Get user info from Google using the access token directly in the header
  response <- GET(
    "https://www.googleapis.com/oauth2/v1/userinfo",
    add_headers(Authorization = paste("Bearer", access_token))
  )
  
  if (status_code(response) == 200) {
    user_info <- fromJSON(rawToChar(response$content))
    return(user_info)
  }
  return(NULL)
}

# Handle the OAuth code exchange and user authentication
handle_google_auth <- function(code, session) {
  # Exchange the authorization code for an access token
  token_response <- POST(
    "https://oauth2.googleapis.com/token",
    body = list(
      code = code,
      client_id = google_client_id,
      client_secret = google_client_secret,
      redirect_uri = google_redirect_uri,
      grant_type = "authorization_code"
    ),
    encode = "form"
  )
  
  if (status_code(token_response) == 200) {
    token_data <- fromJSON(rawToChar(token_response$content))
    access_token <- token_data$access_token
    
    # Get user info using the access token
    user_info <- get_google_user_info(access_token)
    
    if (!is.null(user_info)) {
      # Connect to database
      con <- dbConnect(RSQLite::SQLite(), "users.db")
      
      # Check if user exists
      existing_user <- dbGetQuery(con, sprintf(
        "SELECT * FROM users WHERE google_id = '%s' OR email = '%s'",
        user_info$id, user_info$email
      ))
      
      if (nrow(existing_user) == 0) {
        # Create new user
        dbExecute(con, 
          "INSERT INTO users (username, email, google_id, google_email, profile_picture) 
           VALUES (?, ?, ?, ?, ?)",
          params = list(
            user_info$email,
            user_info$email,
            user_info$id,
            user_info$email,
            user_info$picture
          )
        )
        
        # Get the newly created user
        user <- dbGetQuery(con, sprintf(
          "SELECT * FROM users WHERE google_id = '%s'",
          user_info$id
        ))
      } else {
        # Update existing user's Google info
        dbExecute(con,
          "UPDATE users SET 
           google_id = ?, 
           google_email = ?, 
           profile_picture = ? 
           WHERE email = ?",
          params = list(
            user_info$id,
            user_info$email,
            user_info$picture,
            user_info$email
          )
        )
        user <- existing_user
      }
      
      dbDisconnect(con)
      
      # Return user data
      return(list(
        username = user_info$email,
        email = user_info$email,
        profile_picture = user_info$picture
      ))
    }
  }
  return(NULL)
}

# Server logic
server <- function(input, output, session) {
    # Initialize page state
  appState <- reactiveVal("landing")
  user_data <- reactiveVal(NULL)

  # Setup Google Authentication
  setup_google_auth()

   # Handle Google Sign-In button click
  observeEvent(input$google_signin, {
    # Create OAuth URL
    auth_url <- paste0(
      "https://accounts.google.com/o/oauth2/v2/auth?",
      "client_id=", google_client_id,
      "&redirect_uri=", URLencode("http://localhost:6740", reserved = TRUE),
      "&response_type=code",
      "&scope=", URLencode("email profile openid https://www.googleapis.com/auth/userinfo.profile https://www.googleapis.com/auth/userinfo.email", reserved = TRUE)
    )
    
    # Redirect to Google login
    session$sendCustomMessage("redirectToGoogle", auth_url)
  })
  
  # Handle the OAuth callback observer
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$code)) {
      # Handle the authorization code
      user <- handle_google_auth(query$code, session)
      
      if (!is.null(user)) {
        user_data(user)
        appState("dashboard")
      } else {
        output$login_message <- renderText("Failed to authenticate with Google")
        appState("login")
      }
    }
  })

  # Handle page transitions
  observeEvent(input$getStarted, {
    appState("login")
  })
  
  observeEvent(input$go_to_signup, {
    appState("signup")
  })
  
  observeEvent(input$go_to_login, {
    appState("login")
  })
  
  # Handle signup
  observeEvent(input$signup_btn, {
    req(input$signup_username, input$signup_password, input$signup_confirm_password, input$signup_email)
    
    if (input$signup_password != input$signup_confirm_password) {
      output$signup_message <- renderText("Passwords do not match!")
      return()
    }
    
    tryCatch({
      con <- dbConnect(RSQLite::SQLite(), "users.db")
      
      # Check if username or email already exists
      existing_user <- dbGetQuery(con, sprintf("SELECT * FROM users WHERE username = '%s' OR email = '%s'",
                                             input$signup_username, input$signup_email))
      
      if (nrow(existing_user) > 0) {
        output$signup_message <- renderText("Username or email already exists!")
        dbDisconnect(con)
        return()
      }
      
      # Insert new user
      password_hash <- hash_password(input$signup_password)
      dbExecute(con, "INSERT INTO users (username, password, email) VALUES (?, ?, ?)",
               list(input$signup_username, password_hash, input$signup_email))
      
      dbDisconnect(con)
      output$signup_message <- renderText("Sign up successful! Please login.")
      appState("login")
      
    }, error = function(e) {
      output$signup_message <- renderText("An error occurred during signup.")
    })
  })
  
  # Handle login
  observeEvent(input$login_btn, {
    req(input$login_username, input$login_password)
    
    tryCatch({
      con <- dbConnect(RSQLite::SQLite(), "users.db")
      user <- dbGetQuery(con, sprintf("SELECT * FROM users WHERE username = '%s'", input$login_username))
      dbDisconnect(con)
      
      if (nrow(user) == 1 && verify_password(input$login_password, user$password)) {
        user_data(list(username = user$username, email = user$email))
        appState("dashboard")
      } else {
        output$login_message <- renderText("Invalid username or password!")
      }
      
    }, error = function(e) {
      output$login_message <- renderText("An error occurred during login.")
    })
  })

  # Render the appropriate page based on state
  output$currentPage <- renderUI({
    switch(appState(),
           "landing" = landingPageUI(),
           "login" = loginPageUI(),
           "signup" = signupPageUI(),
           "dashboard" = if (!is.null(user_data())) dashboardUI() else loginPageUI()
    )
  })

  # Original dashboard server logic - only runs when user is authenticated
  observe({
    req(user_data())
    # Your existing dashboard server code here
    data <- stock_data()
    if (!is.null(data)) {
      updateSelectInput(session, "stock", choices = unique(data$Index), selected = unique(data$Index)[1])
      updateDateRangeInput(session, "date_range", start = min(data$Date), end = max(data$Date))
    }
  })

  # Create template dataset function
  create_template_dataset <- function() {
    # Create sample dates
    dates <- seq(as.Date("2024-01-01"), as.Date("2024-01-05"), by="days")
    # Create template dataframe
    template_df <- data.frame(
      Index = rep("COMPNAY_NAME", 5),
      Date = dates,
      Open = rep(100, 5),
      High = rep(105, 5),
      Low = rep(95, 5),
      Close = rep(102, 5),
      AdjClose = rep(102, 5),
      Volume = rep(1000000, 5),
      CloseUSD = rep(102, 5)
    )
    return(template_df)
  }
  
  # Function to write template to temp file and get path
  get_template_path <- function() {
    # Create template
    template_df <- create_template_dataset()
    
    # Create temp file
    tmp <- tempfile(fileext = ".csv")
    write.csv(template_df, tmp, row.names = FALSE)
    
    return(tmp)
  }

  output$downloadTemplate <- downloadHandler(
    filename = function() {
      paste("stock_data_template_", format(Sys.Date(), "%Y%m%d"), ".csv", sep="")
    },
    content = function(file) {
      template_path <- get_template_path()
      file.copy(template_path, file)
      unlink(template_path)
    }
  )

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
