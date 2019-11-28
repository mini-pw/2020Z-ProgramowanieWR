library(shiny)
library(shinycssloaders)
library(shinyhelper)

plotOutputDownload <- function(plotO,butO){
  list(withSpinner(helper(plotOutput(plotO), content = "plot", type = "markdown")),
       downloadButton(butO,butO))
}

ui <- pageWithSidebar(
  headerPanel("Interactive Histogram"),
  sidebarPanel(
    numericInputHelper("lambda", "Lambda", 
                       min = 1, value = 2)
  ),
  mainPanel(
    plotOutputDownload("gestosc","download1"),
    plotOutputDownload("histogram","download2")
  )
)


server <- function(input, output) {
  
  observe_helpers(withMathJax = TRUE, help_dir = "markdowny_pomocowe")
  
  data <- reactive({ 
    rpois(10e6,input[["lambda"]])
  })
  output[["gestosc"]] <- renderPlot({
    plot(density(data()))
  })
  
  output[["histogram"]] <- renderPlot({
    hist(data())
  })
  
  output$download1 <- downloadHandler(
    filename = "Shinyplot.png",
    content = function(file) {
      png(file)
      plot(density(data()))
      dev.off()
    }) 
  
  output$download2 <- downloadHandler(
    filename = "Shinyplot2.png",
    content = function(file) {
      png(file)
      hist(data())
      dev.off()
    }) 
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)