library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Shiny App with Word Report"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("cyl", "Cylinders", choices = unique(mtcars$cyl), selected = 4),
      selectInput("gear", "Gears", choices = unique(mtcars$gear), selected = 4),
      sliderInput("mpg", "MPG greater than:", min = min(mtcars$mpg), max = max(mtcars$mpg), value = 20),
      textInput("report_title", "Report Title", "My Word Report"),
      downloadButton("downloadReport", "Download Word Report")
    ),
    
    mainPanel(
      plotOutput("plot1"),
      plotOutput("plot2"),
      tableOutput("table1")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    mtcars %>%
      filter(cyl == input$cyl,
             gear == input$gear,
             mpg > input$mpg)
  })
  
  output$plot1 <- renderPlot({
    ggplot(filtered_data(), aes(x = mpg, y = hp)) +
      geom_point(color = "blue", size = 10) +
      ggtitle("MPG vs HP")
  })
  
  output$plot2 <- renderPlot({
    ggplot(filtered_data(), aes(x = wt, y = qsec)) +
      geom_point(color = "darkgreen", size = 10) +
      ggtitle("Weight vs 1/4 Mile Time")
  })
  
  output$table1 <- renderTable({
    head(filtered_data(), 10)
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0("report_", Sys.Date(), ".docx")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(
        data = filtered_data(),
        title = input$report_title
      )
      
      rmarkdown::render(tempReport,
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}

shinyApp(ui = ui, server = server)
