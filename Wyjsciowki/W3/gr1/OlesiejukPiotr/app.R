# adapted from https://gist.github.com/garrettgman/6673251

library(shiny)
library(shinycssloaders)
library(shinyhelper)


func_vec <- c("plotOutput", "tableOutput")
func_list <- lapply(func_vec, function(ith_fun) 
  function(outputId, ...) {
    withSpinner(getFromNamespace(ith_fun, ns = "shiny")(outputId = outputId, ...))
  })

for(ith_fun_id in 1L:length(func_list)) {
  assign(x = paste0(func_vec[ith_fun_id], "Spinner"), value = func_list[[ith_fun_id]])
}

ui <- pageWithSidebar(
  headerPanel("Interactive Density Plot"),
  sidebarPanel(
    sliderInput("lambda", "Lambda value", 
                min = 1, max = 10, value = 3)
  ),
  mainPanel(
    plotOutputSpinner("density"),
    tableOutputSpinner("table")
  )
)


server <- function(input, output) {
  
  data <- reactive({ rpois(1000000, input$lambda) })
  
  output[["density"]] <- renderPlot({
    plot(density(data()), main = 'Density Poisson')
  })
  
  output[["table"]] <- renderTable({
    table(data())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
