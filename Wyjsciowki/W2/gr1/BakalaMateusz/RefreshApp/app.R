#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Apka do przypomnienia sobie podstaw"),
    column(6,
           fileInput("csvFile", "Podaj plik .csv",
                     accept = c(
                         "text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
           ),
           verbatimTextOutput("summary"),
           DT::DTOutput("table")
    ),
    column(6,
           verbatimTextOutput("session")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$summary <- renderPrint(
        summary(input$fileInput)
    )
    output$table <- DT::renderDataTable(input$fileInput, editable = "cell")
    output$session <- renderPrint(
        sessionInfo()
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
