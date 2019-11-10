library(shiny)
library(ggplot2)
library(plotly)
library(htmlwidgets)

plot_dat <- data.frame(x = 1L:10, y = 10L:1)

ui <- basicPage(
  plotOutput("points_plot", click = "plot_click"),
  tableOutput("info")
)

server <- function(input, output, session) {
  
  output[["points_plot"]] <- renderPlot({
    
    ggplot(plot_dat, aes(x = x, y = y)) +
      geom_point()
    
  })
  
  values <- reactiveVal(data.frame(x=0, y=0))
  
  observeEvent(input$plot_click,{
    
    old_values <- values()
    # tab <- data.frame(input$plot_click$x, input$plot_click$y)
    new_values <- data.frame(x=input$plot_click$x, y=input$plot_click$y)
    # attach the new line to the old data frame here:
    new_df <- rbind(old_values, new_values)
    
    #store the result in values variable
    values(new_df)
    
  })
  
  output$info <- renderTable({
    return(values())
  })
  
 
}

shinyApp(ui = ui, server = server)

