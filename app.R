library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(readxl)
library(DT)
library(plotly)
library(formattable)


# Load Dataset----
service_level <- read_excel("import_data/Service Level.xlsx")

# UI----
ui <- fluidPage(
  
  themeSelector(),
  useShinydashboard(),
  
  navbarPage(
    title = "PSM Dashboard",
    tabPanel(
      title = "Service Level",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "customercode",
            label = "Select Costumer Code",
            choices = c("All", unique(service_level$Customer)),
            selected = "All"
          ),
          selectInput(
            inputId = "branchname",
            label = "Select Branch Name",
            choices = c("All", unique(service_level$Branch)),
            selected = "All"
          ),
          dateRangeInput(
            inputId = "dates",
            label = "Select Date",
            start = min(service_level$Month),
            end = max(service_level$Month)
          ),
          textOutput("cutoffdate")
        ),
        mainPanel(
          fluidRow(
            valueBoxOutput("servicelevelpercentage"),
            valueBoxOutput("servicelevelvalue", width = 5) 
          ),
          fluidRow(
            plotlyOutput("servicelevelplot")
          )
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
  
  # Reactive Expression to Filter Service Level Data----
  service_level_filtered <- reactive({
    if (input$customercode != "All") {
      service_level <- service_level %>% 
        filter(Customer == input$customercode)
    }
    
    if (input$branchname != "All") {
      service_level <- service_level %>% 
        filter(Branch == input$branchname)
    }
    
    #service_level <- service_level %>% 
      #filter(OpenDate >= input$dates[1] & OpenDate <= input$dates[2])
    
    service_level
  })
  
  # Output Service Level Percentage----
  output$servicelevelpercentage <- renderValueBox({
    servicelvlpct <- 
      service_level_filtered() %>% 
      summarize(sumprocessed = sum(Processed), sumfilled = sum(Filled)) %>% 
      ungroup() %>% 
      mutate(percentage = sumfilled / sumprocessed * 100) %>% 
      select(percentage)
    valueBox(
      paste(round(servicelvlpct, digits = 1), " %"),
      subtitle = "Service Level Percentage",
      color = "blue"
    )
  })
  
  # Output Service Level Value----
  output$servicelevelvalue <- renderValueBox({
    servicelvlvalue <- 
      service_level_filtered() %>% 
      summarize(totalvalue = sum(Value))
    valueBox(
      paste("Rp", accounting(servicelvlvalue, digits = 0L)),
      subtitle = "Service Level Value",
      color = "blue"
    )
  })
  
  # Output Service Level Plot----
  output$servicelevelplot <- renderPlotly({
    service_level %>% 
      group_by(Branch) %>% 
      summarize(sumprocessed = sum(Processed), sumfilled = sum(Filled)) %>% 
      ungroup() %>% 
      mutate(Branch = fct_reorder(Branch, sumprocessed, .desc = TRUE)) %>% 
      plot_ly(x = ~Branch, y = ~sumprocessed, type = "bar", name = "Processed") %>% 
      add_trace(y = ~sumfilled, name = "Filled") %>% 
      layout(yaxis = list(title = 'Count'), barmode = 'group')
  })
  
  # Output Cut Off Date----
  output$cutoffdate <- renderText({
    paste(
      "Cut Off Date is ", as.Date(max(service_level$Month))
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)