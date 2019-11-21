# adapted from https://gist.github.com/garrettgman/6673251

library(shiny)

plotOutputDownload <- function(name) {
    fluidRow(
        plotOutput(name),
        downloadButton(outputId = paste0(name, "Download"), label = "Download the plot")
    )
}

ui <- pageWithSidebar(
    headerPanel("Interactive Histogram"),
    sidebarPanel(
        numericInput("lambda", "Lambda", value=10)
    ),
    mainPanel(
        plotOutputDownload("histogram"),
        plotOutputDownload("density")
    )
)


server <- function(input, output) {
    
    data <- reactive({ 
        rpois(1000, input[["lambda"]])
    })
    
    histogram_plot <- reactive({
        hist(data())
    })
    
    density_plot <- reactive({
        density(data())
    })
    
    output[["histogram"]] <- renderPlot({
        histogram_plot()
    })
    
    output[["density"]] <- renderPlot({
        plot(density_plot())
    })
    
    output[["histogramDownload"]] = downloadHandler(
        filename = 'histogram.png',
        content = function(file) {
            png(file)
            plot(histogram_plot())
            dev.off()
        })
    
    output[["densityDownload"]] = downloadHandler(
        filename = 'density.png',
        content = function(file) {
            png(file)
            plot(density_plot())
            dev.off()
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
