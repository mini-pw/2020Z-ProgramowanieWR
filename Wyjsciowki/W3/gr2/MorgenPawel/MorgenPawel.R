library(shiny)
library(shinycssloaders)
library(ggplot2)

# hist i density dla Poissona miliona punktów i lambdzie ustalanej
# plotOutputDownload - 

plotOutputDownload <- function(outputId, ...){
  list(plotOutput(outputId, ...),
       downloadButton(paste0("button", outputId), "Download img"))
}

# plotOutputDownload2 <- function(outputId, ...){
#   list(plotOutput(outputId, ...),
#        downloadButton("buttonDens", "Download img"))
# }

ui <- pageWithSidebar(
  headerPanel("Wyjsciowka 3"),
  sidebarPanel(
    numericInput("lambda", "Choose lamda", 
                 min = 1, value = 2)
  ),
  mainPanel(
    withSpinner(plotOutputDownload("histogram")),
    withSpinner(plotOutputDownload("density"))
  )
)


server <- function(input, output) {
  
  data <- reactive({ 
    rpois(10e6, input[["lambda"]])
  })
  
  output[["histogram"]] <- renderPlot({
    hist(data(), main = "Histogram", xlab = "data")
  })
  
  output[["density"]] <- renderPlot({
    plot(density(data()), main = "Density plot", xlab = "data")
  })
  
  # observeEvent(input[["buttonHist"]], {
  #   jpeg('rplot.jpg')
  #   hist(data())
  #   dev.off()
  # })
  # 
  output$buttonhistogram <- downloadHandler(
    filename = function() {
      paste0('hist-', Sys.Date(), '.png')
    },
    content = function(con) {
      png(filename = con)
      hist(data(), main = "Histogram", xlab = "data")
      dev.off()
    }
  )
  output$buttondensity <- downloadHandler(
    filename = function() {
      paste0('density-', Sys.Date(), '.png')
    },
    content = function(con) {
      png(filename = con)
      plot(density(data()), main = "Density plot", xlab = "data")
      dev.off()
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

