#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

## Only run examples in interactive R sessions


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
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
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