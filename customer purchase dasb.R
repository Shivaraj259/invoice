library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(ggplot2)

# --- Sample Data Creation ---
# Customer Table
customer_table <- tibble(
  uid = 1:10,
  Name = LETTERS[1:10],
  Ph_no = sample(6366289343:9845807694, 10),
  Add = paste0(letters[1:10], sample(letters, 10), sample(letters, 10))
)

# Purchase Table
purchase_table <- tibble(
  pid = 1:100,
  item = paste0("ITEM_", sample(LETTERS[21:30], 100, replace = TRUE)),
  price = sample(200:900, 100, replace = TRUE),
  quantity = sample(20:90, 100, replace = TRUE),
  uid = sample(1:10, 100, replace = TRUE)
)

# --- UI ---
ui <- dashboardPage(
  dashboardHeader(title = "Customer-Purchase Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Plots", tabName = "plots", icon = icon("chart-bar")),
      menuItem("Left Join Table", tabName = "left", icon = icon("arrow-left")),
      menuItem("Full Join Table", tabName = "full", icon = icon("arrows-alt")),
      menuItem("Inner Join Table", tabName = "inner", icon = icon("link")),
      menuItem("Original Tables", tabName = "raw", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # --- Plots Tab ---
      tabItem(tabName = "plots",
              fluidRow(
                box(title = "Total Purchase Amount per Customer", width = 6,
                    plotOutput("purchase_per_customer")),
                box(title = "Top 10 Most Purchased Items", width = 6,
                    plotOutput("top_items"))
              ),
              fluidRow(
                box(title = "Price vs Quantity (Colored by Item)", width = 12,
                    plotOutput("scatter_plot"))
              )
      ),
      
      # --- Join Result Tables ---
      tabItem(tabName = "left",
              h3("Left Join Result"),
              DTOutput("left_table")),
      tabItem(tabName = "full",
              h3("Full Join Result"),
              DTOutput("full_table")),
      tabItem(tabName = "inner",
              h3("Inner Join Result"),
              DTOutput("inner_table")),
      
      # --- Original Data Tables ---
      tabItem(tabName = "raw",
              fluidRow(
                box(title = "Customer Table", width = 6, DTOutput("customer_table")),
                box(title = "Purchase Table", width = 6, DTOutput("purchase_table"))
              ))
    )
  )
)

# --- Server ---
server <- function(input, output) {
  
  # Join for plotting
  joined_data <- reactive({
    purchase_table %>%
      left_join(customer_table, by = "uid")
  })
  
  # --- PLOTS ---
  
  output$purchase_per_customer <- renderPlot({
    joined_data() %>%
      mutate(total = price * quantity) %>%
      group_by(Name) %>%
      summarise(total_amount = sum(total)) %>%
      ggplot(aes(x = reorder(Name, -total_amount), y = total_amount, fill = Name)) +
      geom_col() +
      labs(x = "Customer", y = "Total Amount", title = "Total Purchase Amount per Customer") +
      theme_minimal()
  })
  
  output$top_items <- renderPlot({
    joined_data() %>%
      group_by(item) %>%
      summarise(total_qty = sum(quantity)) %>%
      top_n(10, total_qty) %>%
      ggplot(aes(x = reorder(item, total_qty), y = total_qty, fill = item)) +
      geom_col() +
      coord_flip() +
      labs(x = "Item", y = "Quantity Sold", title = "Top 10 Most Purchased Items") +
      theme_minimal()
  })
  
  output$scatter_plot <- renderPlot({
    joined_data() %>%
      ggplot(aes(x = price, y = quantity, color = item)) +
      geom_point(size = 3, alpha = 0.7) +
      labs(x = "Price", y = "Quantity", title = "Price vs Quantity by Item") +
      theme_minimal()
  })
  
  # --- TABLE OUTPUTS ---
  
  output$left_table <- renderDT({
    joined_data()
  })
  
  output$full_table <- renderDT({
    purchase_table %>%
      full_join(customer_table, by = "uid")
  })
  
  output$inner_table <- renderDT({
    purchase_table %>%
      inner_join(customer_table, by = "uid")
  })
  
  output$customer_table <- renderDT({ customer_table })
  output$purchase_table <- renderDT({ purchase_table })
}

# Run the app
shinyApp(ui, server)

