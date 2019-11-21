
library(shiny)
library(shinycssloaders)

plotOutputDownload <- function(plotId, buttonId) {
    fluidRow(
        withSpinner(plotOutput(plotId)),
        downloadButton(buttonId)
    )
}

ui <- fluidPage(

    titlePanel("W3"),

    sidebarLayout(
        sidebarPanel(
            numericInput("lambda", "Input lambda", value=0.3)
        ),

        mainPanel(
           plotOutputDownload("histogramPlot", "histogramDownload"),
           plotOutputDownload("densityPlot", "densityDownload")
        )
    )
)

download_helper <- function(filename, plot_generator) {
    downloadHandler(
        filename = function() {"histogram.png"},
        content = function(file) {
            png(file)
            plot_generator()
            dev.off()
        }
    )
}

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    poiss_data <- reactive({
        rpois(10e6, input[["lambda"]])
    })
   
    output[["densityPlot"]] <- renderPlot({
        plot(density(poiss_data()))
    })
    output[["histogramPlot"]] <-renderPlot({
        hist(poiss_data())
    })
    
    output[["histogramDownload"]] <- download_helper("histogram.png", function() {hist(poiss_data())})
    
    output[["densityDownload"]] <- download_helper("density.png", plot(density(poiss_data())))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
