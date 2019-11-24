# adapted from https://gist.github.com/garrettgman/6673251

library(shiny)
library(shinycssloaders)
library(shinyhelper)

f_names <- c('plotOutput')
plotOutputDownload<- function(outputId){
    fluidPage(
        plotOutput(paste0(outputId,'_plot')),
        downloadButton(outputId, label = "Download")
    )
}

?downloadButton


ui <- pageWithSidebar(
    headerPanel("Interactive Histogram"),
    sidebarPanel(
        numericInput("n", "Generate this many points", 
                     min = 1, value = 10e6),
        numericInput("lambda", "Choose Lambda", 
                     min = 0, value = 1),

        sliderInput("bins", "number of bins", 
                    min = 1, max = 100, value = 50)
    ),
    mainPanel(
        plotOutputDownload("histogram"),
        plotOutputDownload("gestosc")
    )
)


server <- function(input, output) {

    data <- reactive(rpois(input[["n"]], input[["lambda"]]))
    
    output[["histogram"]] <- downloadHandler(
        filename = function() { paste('histogram', '.png', sep='') },
        content = function(file) {
            png(file)
            hist(data(), breaks = input$bins)
            dev.off()
        }
    )
    
    output[["gestosc"]] <- downloadHandler(
        filename = function() { paste('gestosc', '.png', sep='') },
        content = function(file) {
            png(file)
            plot(density(data()))
            dev.off()
        }
    )
    output[["histogram_plot"]]  <- renderPlot({
        hist(data(), breaks = input$bins)
    })
    output[["gestosc_plot"]] <- renderPlot({
        plot(density(data()))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
