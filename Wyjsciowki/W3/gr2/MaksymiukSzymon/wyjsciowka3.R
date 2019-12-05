# adapted from https://gist.github.com/garrettgman/6673251

library(shiny)
library(ggplot2)

plotOutputDownload <- function(plot_type) {
  p <- plotOutput(plot_type)
  d <- downloadButton(paste0(plot_type, "Btn"), label = plot_type)
  list(p, d)
}


ui <- pageWithSidebar(
  headerPanel("Interactive Plots"),
  sidebarPanel(
    numericInput("n", "Choose lambda",
                 min = 1, value = 12),
    sliderInput("bins", "number of bins",
                min = 1, max = 100, value = 50)
  ),
  do.call(mainPanel, c(plotOutputDownload("histogram"), plotOutputDownload("density")))
)


server <- function(input, output) {
  set.seed(123)
  
  output[["histogramBtn"]] <- downloadHandler(
    filename = 'histogram.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plot(hist(rpois(n = 1000000, lambda = input$n), breaks = input$bins)), device = device)
    })
  
  output[["densityBtn"]] <- downloadHandler(
    filename = 'density.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plot(density(rpois(n = 1000000, lambda = input$n))), device = device)
    })
  
  
  output[["histogram"]] <- renderPlot({
    hist(rpois(n = 1000000, lambda = input$n), breaks = input$bins)

  })
  
  output[["density"]] <- renderPlot({
    plot(density(rpois(n = 1000000, lambda = input$n)))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

