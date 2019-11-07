
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
            DT::dataTableOutput("contents")
                    )
    )
)

server <- function(input, output) {
    output$contents <- DT::renderDataTable({
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        read.csv(inFile$datapath, header = input$header)
    })
    
    output$summary <-  DT::renderDataTable({
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        summary(read.csv(inFile$datapath, header = input$header))
    })
}

shinyApp(ui, server)