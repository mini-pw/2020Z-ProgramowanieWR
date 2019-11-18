library(shiny)
library(ggplot2)
library(dplyr)

plot_dat <- data.frame(x = 1L:10, y = 10L:1)

ui <- fluidPage(
  plotOutput("points_plot",
             click = "plot1_click"),
  tableOutput('clicked_points')
)

server <- function(input, output, session) {
  
  points <- reactiveVal(NULL)
  
  output[["points_plot"]] <- renderPlot({
    plot_dat %>% 
    ggplot(aes(x = x, y = y)) +
      geom_point()
    
  })
  
  observeEvent(input$plot1_click, {
    near_points <- nearPoints(plot_dat, input$plot1_click, addDist = TRUE)
    points(rbind(points(), data.frame(x=near_points$x, y = near_points$y) ))
  })
  
  output$clicked_points <- renderTable(points())
  
}

shinyApp(ui = ui, server = server) 