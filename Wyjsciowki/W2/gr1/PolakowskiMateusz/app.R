library(shiny)
library(DT)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      tags$hr(),
      checkboxInput("header", "Header", TRUE),
      tags$hr(),
      tableOutput('summary')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table", DT::DTOutput("contents")),
        tabPanel("Session info", htmlOutput("sessionInfo"))
      )
    )
  )
)

server <- function(input, output) {
  output$contents <- DT::renderDT({
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    datatable(read.csv(inFile$datapath, header = input$header), editable = TRUE)
  })
  output$summary <- renderTable({
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    table <- read.csv(inFile$datapath, header = input$header)
    summary(table)
  })
  output$sessionInfo <- renderPrint({
    capture.output(sessionInfo())
  })
}

shinyApp(ui, server)
