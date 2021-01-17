library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
  themeSelector(),
  navbarPage(
    title = "PSM Dashboard",
    tabPanel(
      title = "Menu A",
      sidebarLayout(
        sidebarPanel(
          sliderInput("bins",
                      "Number of bins:",
                      min = 1,
                      max = 50,
                      value = 30)
        ),
        mainPanel(
          plotOutput("distPlot")
        )
      )
    ),
    tabPanel(
      title = "Menu B"
    ),
    tabPanel(
      title = "Menu C"
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)