library(shiny)


plotOutputDownload <- function(outputId) {
    fluidPage(
        plotOutput(paste0(outputId, "_plot")),
        downloadButton(paste0(outputId, "_download"), label = "Download")
    )
}


ui <- pageWithSidebar(
    headerPanel("Interactive Histogram"),
    sidebarPanel(
        numericInput("lambda", "Select lambda", 
                     min = 1, value = 3)
    ),
    mainPanel(
        plotOutputDownload("histogram"),
        plotOutputDownload("density")
    )
)


server <- function(input, output) {
    n <- reactive(1e6)
    
    data <- reactive({
        rpois(n(), input[["lambda"]])
    })
    
    output[["histogram_plot"]] <- renderPlot({
        hist(data())
    })
    
    output[["density_plot"]] <- renderPlot({
        plot(density(data()))
    })
    
    output[["histogram_download"]] <- downloadHandler(
        filename = function() "histogram.png",
        content = function(file) {
            png(file)
            hist(data())
            dev.off()
        })
    
    output[["density_download"]] <- downloadHandler(
        filename = function() "density.png",
        content = function(file) {
            png(file)
            plot(density(data()))
            dev.off()
        })
}


shinyApp(ui = ui, server = server)