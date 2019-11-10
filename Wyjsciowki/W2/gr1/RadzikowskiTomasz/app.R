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
    
    # Application title
    titlePanel("AAA"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Wybierz CSV",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            dataTableOutput("contents")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$contents <- renderDataTable({
        
        inFile<-input$file1
        if (is.null(inFile))
            return(NULL)
        read.csv(inFile$datapath,sep = inFile$sep,
                 quote = inFile$quote)
        #summary(df)
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



