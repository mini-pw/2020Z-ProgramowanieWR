library(shiny)
library(ggplot2)
library(dplyr)

# plot_dat <- data.frame(x = 1L:10, y = 10L:1)

ui <- fluidPage(
  plotOutput("points_plot", click = "plot_click"),
  tableOutput('table')
)

server <- function(input, output, session) {
  plot_dat <- reactiveVal(data.frame(x = 1L:10, y = 10L:1, s = rep(0.5, 10)))

  points <- reactiveVal(NULL)

  observeEvent(input$plot_click, {
    newValue <- rbind(points(), data.frame(x = input$plot_click$x, y = input$plot_click$y))
    points(newValue)             # rv$value <- newValue

    new_plot_dat <- plot_dat() %>% mutate(s = ifelse(x == input$plot_click$x & y == input$plot_click$y, 1, s))
    print(new_plot_dat)
    plot_dat(new_plot_dat)
    })

  output[["points_plot"]] <- renderPlot({

    ggplot(plot_dat(), aes(x = x, y = y)) +
      geom_point(aes(size = s))

  })

  output$table <- renderTable(points())

}

shinyApp(ui = ui, server = server)
