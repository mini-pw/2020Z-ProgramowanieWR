# adapted from https://gist.github.com/garrettgman/6673251

library(shiny)
library(shinycssloaders)
library(shinyhelper)
library(ggplot2)

plotOutputDownload <- function(plotId, buttonId)
{
  list(plotOutput(plotId),
  downloadButton(buttonId))
}

ui <- pageWithSidebar(
  headerPanel("Interactive Histogram"),
  sidebarPanel(
    numericInput("n", "lambda for poisson",
                       min = 1, value = 10),
  ),
  mainPanel(
    plotOutputDownload("histogram", "guzik1"),
    plotOutputDownload("gestosc", "guzik2")
  )
)


server <- function(input, output) {

  data <- reactive({
    rpois(1e6,input$n)
  })

  plotHist <- reactive({
    hist(data())
  })

  plotDens <- reactive({
    plot(density(data()))
  })

  output$guzik1 <- downloadHandler(
    filename = "histogram.png",
    content = function(file) {
      png(file)
      plotHist()
      dev.off()
    })

  output$guzik2 <- downloadHandler(
    filename = "gestosc.png",
    content = function(file) {
      png(file)
      plotDens()
      dev.off()
    })

  output$histogram <- renderPlot({
    hist(data())
  })

  output$gestosc <- renderPlot({
    plot(density(data()))
  })

}

# Run the application
shinyApp(ui = ui, server = server)
