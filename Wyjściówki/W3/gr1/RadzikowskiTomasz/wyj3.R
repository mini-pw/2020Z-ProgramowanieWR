

library(shiny)
library(shinycssloaders)
library(shinyhelper)

func_vec <- c( "plotOutput","tableOutput")
func_list <- lapply(func_vec, function(ith_fun) 
  function(outputId, ...) {
    withSpinner(getFromNamespace(ith_fun, ns = "shiny")(outputId = outputId, ...))
  })

for(ith_fun_id in 1L:length(func_list)) {
  assign(x = paste0(func_vec[ith_fun_id], "Spinner"), value = func_list[[ith_fun_id]])
}



ui <- pageWithSidebar(
  headerPanel("Interactive Histogram"),
  sidebarPanel(
    
    
    sliderInput("lambda", "lambda", 
                      min = 0, max = 15, value = 1)
  ),
  mainPanel(
    plotOutputSpinner("histogram"),
    tableOutputSpinner("tabela"))
  )



server <- function(input, output) {
  observe_helpers(help_dir = "help_mds")
  data <- reactive({ 
    rpois(1000000, input$lambda)
  })

  
  output[["histogram"]] <- renderPlot({
    plot(density(data()))
  })
  output[["tabela"]] <- renderTable({
    table(data())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)