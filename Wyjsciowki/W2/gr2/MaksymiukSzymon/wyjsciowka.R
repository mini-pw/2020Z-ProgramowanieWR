library(shiny)

plot_dat <- data.frame(x = 1L:10, y = 10L:1)


ui <- fluidPage(
  plotOutput("points_plot", click = "clicked"),
  tableOutput("table")
)

server <- function(input, output) {
  values <- reactiveValues(clicked_points = plot_dat[0, ])
  
  observe({
    values$clicked_points <- rbind(isolate(values$clicked_points), 
                                   nearPoints(plot_dat, input$clicked))
    values$clicked_points <- isolate(values$clicked_points[!(duplicated(values$clicked_points) | 
                                     duplicated(values$clicked_points, 
                                                fromLast = TRUE)), ])
  })
  
  output[["points_plot"]] <- renderPlot({
    ggplot(plot_dat, aes(x = x, y = y)) +
      geom_point(size = 3) +
      geom_point(data = values$clicked_points, size = 6)
  })
  
  output[["table"]] <- renderTable({

    values$clicked_points
  })
}

shinyApp(ui, server)

