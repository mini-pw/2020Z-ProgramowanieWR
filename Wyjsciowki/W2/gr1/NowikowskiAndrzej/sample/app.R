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
            # sliderInput("bins","Number of bins:",min = 1,max = 50,value = 30),
            fileInput("my_csv", "Tutaj podaj proszę csv")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           # plotOutput("distPlot"),
           head("MY CSV Info"),
           dataTableOutput("tabelka"),
           head('CSV Summary'),
           tableOutput("csv_summary"),
           head("Session Info"),
           verbatimTextOutput("urlText")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$tabelka <- renderDataTable({
        if (!is.null(input$my_csv)) {
            DT::datatable(as.data.frame(read.csv(input$my_csv$datapath, header=TRUE)), editable = TRUE)
        }
    })
    
    output$csv_summary <- renderTable({
        if (!is.null(input$my_csv)) {
            summary(as.data.frame(read.csv(input$my_csv$datapath, header=TRUE)))
        }
    })

    # https://shiny.rstudio.com/articles/client-data.html
    output$urlText <- renderText({
        # paste(sep = "",
        #      "protocol: ", session$clientData$url_protocol, "\n",
        #      "hostname: ", session$clientData$url_hostname, "\n",
        #      "pathname: ", session$clientData$url_pathname, "\n",
        #      "port: ",     session$clientData$url_port,     "\n",
        #      "search: ",   session$clientData$url_search,   "\n"
        #)
        paste(sep="\n", sessionInfo())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
