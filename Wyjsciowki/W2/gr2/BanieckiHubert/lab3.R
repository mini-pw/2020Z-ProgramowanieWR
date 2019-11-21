
library(shiny)
library(ggplot2)
library(DT)

plot_dat <- data.frame(x = 1L:10, y = 10L:1, clicked = FALSE, size = 2)

ui <- fluidPage(plotOutput("points_plot", click = "plot_click"),
                verbatimTextOutput("values"),
                DT::dataTableOutput("tb"))

server <- function(input, output, session) {
  values <- reactiveValues()
  
  observe({
    values$data <-data.frame(x = 1L:10, y = 10L:1, clicked = FALSE, size = 2)
  })
  
  output[["points_plot"]] <- renderPlot({
    
    ggplot(values$data, aes(x = x, y = y)) +
      geom_point(aes(size = clicked)) + 
      theme(legend.position = "none") 
    
  })
  output[['values']] <- renderPrint({
    print(values$data)  
  })
  
  output[['tb']] <- DT::renderDataTable({
    
    # inwencja twórcza - nie ze stacka
    odl_x <- abs(plot_dat$x-input$plot_click$x)
    odl_y <- abs(plot_dat$y-input$plot_click$y)
    
    values$data$clicked[odl_x < 0.3 & odl_y < 0.3] <- TRUE
    values$data$size[odl_x < 0.3 & odl_y < 0.3] <- 4 
    
    # kiedy zaznaczy się wszystkie to się zmniejszają, ale nadal mają różny size
    # bo najzwyczajniej zmienia się skalowanie kropek
    if (any(values$data$clicked)) {
      DT::datatable(values$data[values$data$clicked==TRUE,], editable= TRUE) 
    }
  })
  
  
}

shinyApp(ui = ui, server = server)