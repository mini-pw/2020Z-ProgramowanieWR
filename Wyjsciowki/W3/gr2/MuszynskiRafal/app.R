# adapted from https://gist.github.com/garrettgman/6673251

library(shiny)

plotOutputDownload <- function(plotId){
  list(
    plotOutput(plotId),
    downloadButton(paste0(plotId, "_download"), label = paste0("Download ", plotId), "")
  )
}

ui <- pageWithSidebar(
  headerPanel("Interactive poisson"),
  sidebarPanel(
    numericInput("lambda", "Lambda for poisson distribution", min = 0, value = 5)
  ),
  mainPanel(
    plotOutputDownload("histogram"),
    plotOutputDownload("density")
  )
)


server <- function(input, output) {
  
  data <- reactive({ 
    rpois( 1000, input[["lambda"]])
  })
  
  hist_plot <- reactive({
    hist(data())
  })
  
  dens_plot <- reactive({
    density(data())
  })
  
  output[["histogram"]] <- renderPlot({
    hist_plot()
  })
  
  output[["density"]] <- renderPlot({
    plot(dens_plot())
  })
  
  output[['density_download']] <- downloadHandler(
    filename = 'density.png',
    content = function(file) {
      png(file)
      plot(dens_plot())
      dev.off()
    }
  )
  
  output[['histogram_download']] <- downloadHandler(
    filename = 'histogram.png',
    content = function(file) {
      png(file)
      plot(hist_plot())
      dev.off()
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
