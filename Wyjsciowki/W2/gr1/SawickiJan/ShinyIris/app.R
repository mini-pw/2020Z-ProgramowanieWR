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
ui <- basicPage(
    fileInput("file1", "Choose CSV File",
              accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
    ),
    DT::dataTableOutput("irisTable"),
    DT::dataTableOutput("irisSummaryTable"),
    verbatimTextOutput("urlText")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$irisTable = renderDT({
        if(!is.null(input[["file1"]])){
            filename = input[["file1"]]$datapath
        iris2 = read.csv(filename)
        datatable(iris2, selection = 'none', editable = TRUE)
        }
        })
    output$irisSummaryTable = renderDT({
        if(!is.null(input[["file1"]])){
            filename = input[["file1"]]$datapath
        iris2 = read.csv(filename)
        datatable(summary(iris2), selection = 'none', editable = TRUE)
        }
    })

    output$urlText <- renderText({
        paste(
            sep = "",
            "protocol: ",
            session$clientData$url_protocol,
            "\n",
            "hostname: ",
            session$clientData$url_hostname,
            "\n",
            "pathname: ",
            session$clientData$url_pathname,
            "\n",
            "port: ",
            session$clientData$url_port,
            "\n",
            "search: ",
            session$clientData$url_search,
            "\n"
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
