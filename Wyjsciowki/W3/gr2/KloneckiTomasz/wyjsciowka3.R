library(shiny)

plot_names <- c('density','histogram')
l <- lapply(plot_names, function(ith_plot_name)
  function(ith_plot_name){
    fluidPage(
    plotOutput(ith_plot_name),
    downloadButton(outputId = paste0(ith_plot_name,'_button'), label = 'Download')
    )
    }
)
for(i in 1:length(plot_names)){
  assign(paste0(plot_names[i], 'OutputDownload'), value = l[[i]])
}

ui <- pageWithSidebar(
  headerPanel("Poisson Distribution plots"),
  sidebarPanel(
    numericInput("n", "Select the lambda value of the poisson distribution", 
                 min = 1, value = 10)
  ),
  mainPanel(
    histogramOutputDownload('histogram'),
    densityOutputDownload('density')
    
  )
)


server <- function(input, output) {
  data <- reactive({ 
    rpois(1e6, input$n)
  })
  
  output$histogram <- renderPlot({
    hist(data())
  })
  
  output$density <- renderPlot({
    d <- density(data())
    plot(d)
  })
  
  output$density_button <- downloadHandler(
                            filename = 'density.png',
                            content = function(file) {
                              png(file)
                              d <- density(data())
                              plot(d)
                              dev.off()
  }
  )
  
  output$histogram_button <- downloadHandler(
    filename = 'histogram.png',
    content = function(file) {
      png(file)
      hist(data())
      dev.off()
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server) 