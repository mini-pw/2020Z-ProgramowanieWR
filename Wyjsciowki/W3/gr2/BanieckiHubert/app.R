# adapted from https://gist.github.com/garrettgman/6673251

library(shiny)
library(shinycssloaders)
library(shinyhelper)

n <- 10^6

plotOutputDownload <- function(name) {  
    list(withSpinner(plotOutput(name)), downloadButton(paste0(name,"_download"), "Download figure",
                                                    contentType = "image/png"))
}

ui <- pageWithSidebar(
    headerPanel("Interactive Histogram"),
    sidebarPanel(
        numericInput("lambda", "Lambda", 
                     min = 0.1, value = 5)
    ),
    mainPanel(
        plotOutputDownload("histogram"),
        plotOutputDownload("density")
    )
)


server <- function(input, output) {
    
    observe_helpers(withMathJax = TRUE, help_dir = "markdowny_pomocowe")
    
    data <- reactive({ 
        rpois(n, input[["lambda"]])
    })
    
    output[["histogram"]] <- renderPlot({
        hist(data(), breaks = 11)
    })
    
    output[["density"]] <- renderPlot({
        plot(density(data()))
    })
    
    output[["histogram_download"]] <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".png", sep="")
        },
        content = function(file) {
            png(file)
            hist(data(), breaks = 11)
            dev.off()
        }
    )
    
    output[["density_download"]] <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".png", sep="")
        },
        content = function(file) {
            png(file)
            plot(density(data()))
            dev.off()
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
