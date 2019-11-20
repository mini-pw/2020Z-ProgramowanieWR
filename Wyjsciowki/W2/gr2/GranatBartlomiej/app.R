library(shiny)
library(ggplot2)

plot_dat <- data.frame(x = 1L:10, y = 10L:1)

ui <- fluidPage(
  plotOutput("points_plot", click="plot_click"),
  verbatimTextOutput("info"))

server <- function(input, output, session) {

  rv <- reactiveValues(selected_points = plot_dat[0, ])

  observe({

    rv$selected_points <- rbind(isolate(rv$selected_points),
                                nearPoints(plot_dat, input$plot_click,threshold=100,maxpoints=1))

    rv$selected_points <- isolate(
      rv$selected_points[!(duplicated(rv$selected_points) |
                             duplicated(rv$selected_points, fromLast = TRUE)), ])
    str(rv$selected_points)
  })



  output[["points_plot"]] <- renderPlot({

    ggplot(plot_dat, aes(x = x, y = y)) +
      geom_point() +
      geom_point(data = rv$selected_points, colour = "red", size = 10)

  })

  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })

}

shinyApp(ui = ui, server = server)
