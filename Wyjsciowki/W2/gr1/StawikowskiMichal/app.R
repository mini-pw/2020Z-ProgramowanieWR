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
            checkboxInput("header", "Header", TRUE)
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("SessionInfo", htmlOutput("sessionInfo")), 
                tabPanel("Summary", tableOutput("summary")), 
                tabPanel("Table", DT::DTOutput("contents"))
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

