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
      checkboxInput("header", "Header", TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Informacje", htmlOutput("Informacje")), 
        tabPanel("Podsumowanie", tableOutput("Podsumowanie")), 
        tabPanel("Tabela", DT::DTOutput("Tabela"))
      )
    )
  )
)

server <- function(input, output) {
  output$Tabela <- DT::renderDT({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    datatable(read.csv(inFile$datapath, header = input$header), editable = TRUE)
  })
  output$Podsumowanie <- renderTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    table <- read.csv(inFile$datapath, header = input$header)
    summary(table)
  })
  output$Informacje <- renderPrint({
    capture.output(sessionInfo())
  })
}

shinyApp(ui, server)