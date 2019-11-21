library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Simple Shiny App"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose CSV File", accept = (".csv"),
                      buttonLabel = "Browse...",
                      placeholder = "No file selected")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           dataTableOutput("DT")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output[["DT"]] <- renderDataTable({
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        read.csv(inFile$datapath, sep = inFile$sep, quote = inFile$quote)
    })
    
    
    
    # iris_r <- reactive({
    #     filter(countries, continent %in% input[["chosen_continent"]]) 
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
