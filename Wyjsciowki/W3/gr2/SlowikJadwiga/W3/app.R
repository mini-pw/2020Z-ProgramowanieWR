
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

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    poiss_data <- reactive({
        rpois(10e6, input[["lambda"]])
    })
    
    density_plot_output <- reactive({
        plot(density(poiss_data()))
    })
    
    histogram_plot_output <- reactive({
        hist(poiss_data())
    })
    
    output[["densityPlot"]] <- renderPlot({
        density_plot_output()
    })
    output[["histogramPlot"]] <-renderPlot({
        histogram_plot_output()
    })
    
    output[["histogramDownload"]] <- downloadHandler(
        filename = function() {"histogram.png"},
        content = function(file) {
            png(file)
            hist(poiss_data())
            dev.off()
        }
    )
    output[["densityDownload"]] <- downloadHandler(
        filename = function() {"density.png"},
        content = function(file) {
            png(file)
            plot(density(poiss_data()))
            dev.off()
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
