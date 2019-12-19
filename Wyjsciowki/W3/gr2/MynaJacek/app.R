# adapted from https://gist.github.com/garrettgman/6673251

library(shiny)
library(shinyhelper)

plotOutputDownload <- function(inputId, buttonId){
  fluidRow(
    plotOutput(inputId),
    downloadButton(buttonId, 'Download data')
  )
}


ui <- pageWithSidebar(
  headerPanel("Interactive Density Plot"),
  sidebarPanel(
    sliderInput("lambda", "Lambda value",
                min = 1, max = 10, value = 3)
  ),
  mainPanel(
    plotOutputDownload("density", "d"),
    plotOutputDownload("histogram", "h")
  )
)


server <- function(input, output) {


  densityPlot <- reactive(
    plot(density(data()), main = 'Density Poisson')
  )

  histogramPlot <- reactive(
    plot(hist(data()), main = 'Histogram Poisson')
  )


  output$d <- downloadHandler(
    filename =  function() {
      paste0("density", ".png")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      png(file) # open the png device
      densityPlot() # draw the plot
      dev.off()  # turn the device off

    } )

  output$h <- downloadHandler(
    filename =  function() {
      paste0("histogram", ".png")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      png(file) # open the png device
      histogramPlot() # draw the plot
      dev.off()  # turn the device off

    } )

  data <- reactive({ rpois(1000000, input$lambda) })

  output[["density"]] <- renderPlot({
    plot(density(data()), main = 'Density Poisson')
  })

  output[["histogram"]] <- renderPlot({
    plot(hist(data()), main = 'Histogram Poisson')
  })

}

# Run the application
shinyApp(ui = ui, server = server)
