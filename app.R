library(shiny)
library(tidyverse)
library(shinydashboard)

# keep track of initial data for resetting
start_sales_data <- read_csv("data/sales.csv")

# ui ----------------------------------------------------------------------


ui <- dashboardPage(
  dashboardHeader(title = "Updating Sales"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    sidebarLayout(
      sidebarPanel(
        actionButton("update_background_data", "Update Sales"),
        actionButton("reset_background_data", "Reset Sales Info")
      ),
      mainPanel(
        valueBoxOutput("total_sales"),
        valueBoxOutput("daily_sales"),
        valueBoxOutput("today_date"),
        plotOutput("monthly_sales")
      )
    )
  )
)


# server ------------------------------------------------------------------

server <- function(input, output, session) {
  # polling function - checks (every second if data has been updated)
  # if it has, load in data again
  #
  # note, data is now a reactive object
  sales_data <- reactiveFileReader(
    1000,
    session,
    "data/sales.csv",
    readr::read_csv
  )
  
  # for value boxes
  total_sales <- reactive(
    sum(sales_data()$sales)
  )
  
  daily_sales <- reactive(
    sum((sales_data() %>% filter(date == max(sales_data()$date)))$sales)
  )
  
  todays_date <- reactive(
    max(sales_data()$date)
  )
  
  # render value boxes
  output$total_sales <- renderValueBox({
    valueBox(
      as.character(total_sales()), "Total Sales", icon = icon("credit-card"),
      color = "purple"
    )
  })
  
  output$daily_sales <- renderValueBox({
    valueBox(
      as.character(daily_sales()), "Daily Sales", icon = icon("credit-card"),
      color = "blue"
    )
  })
  
  output$today_date <- renderValueBox({
    valueBox(
      as.character(todays_date()), "Date:", icon = icon("clock"),
      color = "yellow"
    )
  })
  
  # render simple plot
  
  output$monthly_sales <- renderPlot(
    ggplot(sales_data()) +
      aes(x = date, y = sales) +
      geom_col(fill = "steelblue") +
      labs(x = NULL,
           y = "Sales") +
      theme_bw()
  )
  
  
  # when button is pressed, add a new row to sales.csv
  observeEvent(input$update_background_data, {
    # generate a new row of data (an employee has made some sales to record)
    random_id <- sample(11:20, 1, replace = TRUE)
    random_sale <- sample(1:27, 1, replace = TRUE, prob = seq(0.5, 0.1, -0.015))
    date <- Sys.Date()
    
    sales_data() %>% 
      add_row(
        employee_id = paste0("0", random_id),
        date = date,
        sales = random_sale
      ) %>% 
      write_csv("data/sales.csv")
  })
  
  # when reset button is pressed, set data to initial point
  observeEvent(input$reset_background_data, {
    start_sales_data %>% 
      write_csv("data/sales.csv")
  })
  
}


shinyApp(ui, server)