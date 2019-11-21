library(shiny)
library(ggplot2)
library(DT)

plot_dat <- data.frame(x = 1L:10, y = 10L:1)

ui <- fluidPage(
  plotOutput("points_plot", click = "plot1_click"),
  DT::DTOutput("click_info"))

server <- function(input, output, session) {

  output[["points_plot"]] <- renderPlot({
    if(length(values$values)<1)
    {
    ggplot(plot_dat, aes(x = x, y = y)) +
      geom_point()
    }
    else{
      ggplot(plot_dat, aes(x = x, y = y)) +
        geom_point()+
        geom_point(data = values$values, mapping = aes(x = x, y = y),size = 10)
    }

  })
  values <- reactiveValues()

  #
  output$click_info <- DT::renderDT({
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    points <- nearPoints(plot_dat, input$plot1_click, threshold = 10)
    if(length(isolate(values$values))<0)
      values$values <- data.frame(points)
    values$values <- rbind(isolate(values$values), isolate(points))
    datatable(values$values)
  })

  # Clicked_Point1 <- nearPoints(data, input$points_plot, threshold = 10, maxpoints = 1)
  #
  observeEvent(input$plot1_click, {
    # values$data <-
  })
}

shinyApp(ui = ui, server = server)
