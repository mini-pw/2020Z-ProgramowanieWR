library(shiny)
library(ggplot2)

plot_dat <- data.frame(x = 1L:10, y = 10L:1, size=rep(1,10))

ui <- fluidPage(plotOutput("points_plot", height=300,
                           click = "plot_click"), 
                h4("Clicked points"),
                tableOutput("plot_clickedpoints"))

server <- function(input, output, session) {
  data <- reactive({
    plot_dat
  })
  output[["points_plot"]] <- renderPlot({
    
    ggplot(data(), aes(x = x, y = y, size=size)) +
      geom_point()
    
  })
  output$plot_clickedpoints <- renderTable({
    # For base graphics, we need to specify columns, though for ggplot2,
    # it's usually not necessary.
    
    res <- nearPoints(data(), input$plot_click, "x", "y")
    if (nrow(res) == 0)
      return()
    data()[data()['x'] == res['x'] & data()['y'] == res['y']] ['size']= 10
    res
    
  })
  
}

#shinyApp(ui = ui, server = server)

shinyApp(ui=ui, server=server)
