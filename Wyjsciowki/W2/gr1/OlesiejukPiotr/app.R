if (interactive()) {
    
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
                DT::dataTableOutput("contents"),
                h3("Session Info"),
                verbatimTextOutput("session")
            )
        )
    )
    
    server <- function(input, output, session) {
        
        output$contents <- DT::renderDataTable({
            inFile <- input$file1
            
            if (is.null(inFile))
                return(NULL)
            
            datatable(read.csv(inFile$datapath, header = input$header), editable = 'cell')
        })
        output$session <- renderText({
            capture.output(sessionInfo())
        })
    }
    
    shinyApp(ui, server)
}