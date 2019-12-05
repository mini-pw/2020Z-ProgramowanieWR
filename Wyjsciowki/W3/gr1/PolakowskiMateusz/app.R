library(shiny)
library(shinycssloaders)
library(shinyhelper)

f_names <- c('tableOutput', 'plotOutput')

inputHelpers <- lapply(f_names, function(f_name)
  function(outputId, ...) {
    withSpinner(getFromNamespace(f_name, ns = 'shiny') (outputId = outputId, ...))
  })

for (i in 1:length(inputHelpers)) {
  assign(x = paste0(f_names[i], "Spinner"), value = inputHelpers[[i]])
}

ui <- pageWithSidebar(
  headerPanel("Interactive Histogram"),
  sidebarPanel(
    numericInput("lambda", "lambda",  min = 0.01, max = 100, value = 2),
    sliderInput("bins", "number of bins", 
                min = 1, max = 100, value = 5)
  ),
  mainPanel(
    plotOutputSpinner('histogram_pois'),
    tableOutputSpinner('table_pois')
  )
)


server <- function(input, output) {
  
  observe_helpers(help_dir = 'help_mds')
  
  data_pois <- reactive({
    rpois(10000, input[['lambda']])
  })
  
  output[["histogram_pois"]] <- renderPlot({
    hist(data_pois(), breaks = input$bins)
  })
  
  output[["table_pois"]] <- renderTable({
    table(data_pois())
  })
}

# Run the application
shinyApp(ui = ui, server = server)