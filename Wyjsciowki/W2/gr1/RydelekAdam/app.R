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

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Tabsets"),
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
      tabsetPanel(
        tabPanel("SessionInfo", verbatimTextOutput("sessionInfo")), 
        tabPanel("Summary", tableOutput("summary")), 
        tabPanel("Table", DT::DTOutput("table"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$table <- DT::renderDT({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    datatable(read.csv(inFile$datapath, header = input$header), editable=TRUE)
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
  render_dt = function(data, editable = 'cell', server = TRUE, ...) {
    renderDT(data, selection = 'none', server = server, editable = editable, ...)
  }
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

