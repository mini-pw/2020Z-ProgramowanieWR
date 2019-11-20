library(shiny)
library(ggplot2)

plot_dat <- data.frame(x = 1L:10, y = 10L:1)

ui <- fluidPage(
  plotOutput("points_plot", click = "point_click"),
  verbatimTextOutput("info")
)

server <- function(input, output, session) {
  
  selected_points <- reactiveValues(indices = c())
  
  observeEvent(input[["point_click"]],{
    nearest_point <- nearPoints(plot_dat, input$point_click, maxpoints = 1)[["ind"]]
    
    if(length(nearest_point == 1)) {
      if(nearest_point %in% selected_points[["indices"]])
        selected_points[["indices"]] <- selected_points[["indices"]][!selected_points[["indices"]] %in% nearest_point]
      else
        selected_points[["indices"]] <- unique(selected_points[["indices"]])
    }
    
  })
  
  output[["points_plot"]] <- renderPlot({
    
    ggplot(plot_dat, aes(x = x, y = y), size = ifelse(selected_points[["indices"]] %in% 1:length(plot_dat)),
           1,
           2) +
      geom_point() 
    
  })

   output[["df"]] <- renderTable({
     plot_dat[selected_points[["indices"]], ]
   })
  # output$info <- renderPrint({
  #   selected_points = list()
  #   if(!is.null(input[["point_click"]])) append(selected_points, nearPoints(plot_dat, input$point_click))
  #   selected_points
  # })
}

shinyApp(ui = ui, server = server)
