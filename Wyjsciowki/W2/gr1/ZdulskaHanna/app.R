library(shiny)

ui <- fluidPage(
  titlePanel("Wyjsciowka2"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      tags$hr(),
      checkboxInput("header", "Header", TRUE)
    ),
    mainPanel(
      tableOutput("contents")
    )
  )
)


server <- function(input, output) {
  output$contents <- renderTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    summary(read.csv(inFile$datapath, header = input$header))
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
