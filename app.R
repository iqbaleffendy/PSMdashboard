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
data_BO_raw <- read_excel("import_data/BO 15.xlsx")
data_BO <- data_BO_raw %>%
  filter(!is.na(Group)) %>%
  separate(Customer, into = c("CustomerCode", "separator", "CustomerName"), sep = c(8,9), remove = TRUE) %>% 
  select(-separator) %>% 
  group_by(Agc, Partno, Description, Group, CustomerCode, CustomerName) %>% 
  summarize(
    Total_Qty = sum(`BO Qty`),
    Max_Days = max(`Aging Day`)
  ) %>% 
  ungroup()


ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  useShinydashboard(),
  
  navbarPage(
    title = "PSM Dashboard",
    position = "fixed-top",
    selected = "Service Level",
    tags$style(type="text/css", "body {padding-top: 70px;}"),
    tabPanel(
      title = "Service Level",
      sidebarLayout(
        sidebarPanel(
          width = 3,
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
          width = 9,
          tabsetPanel(
            tabPanel(
              title = "Summary",
              fluidRow(
                column(
                 width = 4,
                 offset = 2,
                 valueBoxOutput("servicelevelpercentage", width = NULL)
                ),
                column(
                  width = 5,
                  offset = 0,
                  valueBoxOutput("servicelevelvalue", width = NULL)
                )
              ),
              fluidRow(
                plotlyOutput("servicelevelplot")
              )
            ),
            tabPanel(
              title = "Dataset",
              DTOutput("serviceleveldataset")
            )
          )
        )
      )
    ),
    tabPanel(
      title = "Back Order",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(
            inputId = "groupname",
            label = "Select Group Name",
            choices = c("All", unique(data_BO$Group)),
            selected = "All"
          ),
          selectInput(
            inputId = "customername",
            label = "Select Customer Name",
            choices = c("All", unique(data_BO$CustomerCode)),
            selected = "All"
          ),
          sliderInput(
            inputId = "BODays",
            label = "Select Range Days",
            min = min(data_BO$Max_Days),
            max = max(data_BO$Max_Days),
            value = c(min(data_BO$Max_Days), max(data_BO$Max_Days))
          )
        ),
        mainPanel(
          width = 9,
          DTOutput("BODataset")
        )
      )
    ),
    tabPanel(
      title = "Source Code",
      box(
        title = "Source Code",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        pre(includeText("app.R"))
      )
    )
  )
)


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
      subtitle = tags$p("Service Level Percentage", style = "font-size: 110%"),
      color = "light-blue",
      icon = icon("list")
    )
  })
  
  # Output Service Level Value----
  output$servicelevelvalue <- renderValueBox({
    servicelvlvalue <- 
      service_level_filtered() %>% 
      summarize(totalvalue = sum(Value))
    valueBox(
      paste("Rp", accounting(servicelvlvalue, digits = 0L)),
      subtitle = tags$p("Service Level Value", style = "font-size: 110%"),
      color = "light-blue",
      icon = icon("list")
    )
  })
  
  # Function for Service Level Plot----
  servicelevelplot <- function(data, group) {
    data %>% 
      group_by({{group}}) %>% 
      summarize(sumprocessed = sum(Processed), sumfilled = sum(Filled)) %>% 
      ungroup() %>% 
      mutate(group = fct_reorder({{group}}, sumprocessed, .desc = TRUE)) %>% 
      plot_ly(x = ~group, y = ~sumprocessed, type = "bar", name = "Processed") %>% 
      add_trace(y = ~sumfilled, name = "Filled") %>% 
      layout(yaxis = list(title = 'Count'), barmode = 'group') %>%
      layout(xaxis = list(title = 'Branch/Customer')) %>% 
      layout(legend = list(x = 0.8, y = 0.8, bgcolor = "#E2E2E2"))
  }
  
  # Output Service Level Plot----
  output$servicelevelplot <- renderPlotly({
    if (input$branchname == "All") {
      servicelevelplot(service_level, Branch)
    } else {
      servicelevelplot(service_level_filtered(), Customer)
    }
  })
  
  # Output Cut Off Date----
  output$cutoffdate <- renderText({
    paste(
      "Cut Off Date is ", as.Date(max(service_level$Month))
    )
  })
  
  # Output Service Level Dataset----
  output$serviceleveldataset <- renderDT({
    datatable(
      service_level_filtered() %>% select(1,3,4,5,7,8,11),
      class = 'cell-border stripe'
    )
  })
  
  # Reactive Expression to Filter BO Dataset----
  data_BO_filtered <- reactive({
    if (input$groupname != "All") {
      data_BO <- data_BO %>% 
        filter(Group == input$groupname)
    }
    
    if(input$customername != "All") {
      data_BO <- data_BO %>% 
        filter(CustomerCode == input$customername)
    }
    
    data_BO <- data_BO %>% 
      filter(Max_Days >= input$BODays[1] & Max_Days <= input$BODays[2])
    
    data_BO
  })
  
  #Output Back Order Dataset----
  output$BODataset <- renderDT({
    datatable(
      data_BO_filtered() %>% 
        select(-4) %>% 
        arrange(desc(Max_Days)),
      class = 'cell-border stripe'
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)