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

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose CSV File"),
            tags$hr(),
            checkboxInput("header", "Header", TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            DTOutput('table')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

        output$table <- renderDT({
            # input$file1 will be NULL initially. After the user selects
            # and uploads a file, it will be a data frame with 'name',
            # 'size', 'type', and 'datapath' columns. The 'datapath'
            # column will contain the local filenames where the data can
            # be found.
            inFile <- input$file1
            
            if (is.null(inFile))
                return(NULL)
            
            read.csv(inFile$datapath, header = input$header)
      
        })

    }


# Run the application 
shinyApp(ui = ui, server = server)
