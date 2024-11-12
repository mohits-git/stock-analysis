# R Shinny application
library(shiny)

# User Interface
ui <- fluidPage(
  titlePanel("Basic Shiny App"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("num", 
                  "Choose a number:", 
                  min = 1, max = 100, value = 25)
    ),
    mainPanel(
      plotOutput("histPlot")
    )
  )
)

# Server Logic
server <- function(input, output) {
  output$histPlot <- renderPlot({
    hist(rnorm(input$num))
  })
}

# Combine UI and server to create the app
shinyApp(ui = ui, server = server)