library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)

# Sample data (you can replace this with your own)
# Comment this part if you're using your own data
purchase_table <- data.frame(
  uid = c(1, 2, 3, 5),
  item = c("Book", "Pen", "Notebook", "Pencil"),
  amount = c(10, 2, 5, 1)
)

customer_table <- data.frame(
  uid = c(1, 2, 4),
  name = c("Alice", "Bob", "Charlie"),
  email = c("alice@example.com", "bob@example.com", "charlie@example.com")
)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Join Demo Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Left Join", tabName = "left", icon = icon("arrow-left")),
      menuItem("Full Join", tabName = "full", icon = icon("arrows-alt")),
      menuItem("Inner Join", tabName = "inner", icon = icon("equals"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "left",
              h3("Left Join: All purchases with customer info (if available)"),
              DTOutput("left_join_table")
      ),
      tabItem(tabName = "full",
              h3("Full Join: All purchases and all customers"),
              DTOutput("full_join_table")
      ),
      tabItem(tabName = "inner",
              h3("Inner Join: Only matching users with purchases"),
              DTOutput("inner_join_table")
      )
    )
  )
)

# Define server
server <- function(input, output) {
  
  output$left_join_table <- renderDT({
    purchase_table %>%
      left_join(customer_table, by = "uid")
  })
  
  output$full_join_table <- renderDT({
    purchase_table %>%
      full_join(customer_table, by = "uid")
  })
  
  output$inner_join_table <- renderDT({
    purchase_table %>%
      inner_join(customer_table, by = "uid")
  })
}

# Run the app
shinyApp(ui, server)
