library(shiny)
library(shinycssloaders)
library(shinyhelper)

func_vec <- c("plotOutput", "tableOutput")
func_list <- lapply(func_vec, function(ith_fun) 
    function(outputId, ...) {
        withSpinner(getFromNamespace(ith_fun, ns = "shiny")(outputId = outputId, ...))
    })

for(ith_fun_id in 1:length(func_list)) {
    assign(x = paste0(func_vec[ith_fun_id], "Spinner"), value = func_list[[ith_fun_id]])
}

ui <- pageWithSidebar(
    headerPanel("Interactive Histogram"),
    sidebarPanel(
        numericInput("n", "Select the lambda value of the poisson distribution", 
                     min = 1, value = 10)
    ),
    mainPanel(
        plotOutputSpinner("histogram"),
        tableOutputSpinner("frequencyTable")
    )
)


server <- function(input, output) {
    data <- reactive({ 
        rpois(1e6, input$n)
    })
    
    output$histogram <- renderPlot({
      d <- density(data())
      plot(d)
    })
    
    output$frequencyTable <- renderTable({
      table(data())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)