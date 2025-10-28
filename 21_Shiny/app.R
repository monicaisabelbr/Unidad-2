# install.packages("shiny")
library(shiny)


# Define UI
ui <- fluidPage(
  titlePanel("Hello shinyapps.io!"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("num", "Choose a number:", 
                  min = 1, max = 100, value = 50)
    ),
    mainPanel(
      textOutput("result"),
      plotOutput("histPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$result <- renderText({
    paste("You chose the number:", input$num)
  })
  
  output$histPlot <- renderPlot({
    hist(rnorm(input$num), col = "steelblue", main = "Histogram of Random Values")
  })
}

# Run the app
shinyApp(ui = ui, server = server)

