library(shiny)
library(shinycssloaders)
library(shinyhelper)



ui <- pageWithSidebar(
  headerPanel("Interactive Histogram"),
  sidebarPanel(
    numericInput("n", "Generate this many points", 
                       min = 1, value = 100),
    sliderInput("lambda", "Select lambda", 
                min = 1, max = 100, value = 1)
  ),
  mainPanel(
    plotOutput("plot")
  )
  
)


server <- function(input, output) {
  
  data <- reactive({ 
    
    rpois(input[["n"]], input[["lambda"]])
  })
  
  output[["plot"]] <- renderPlot({
    hist(data())
  })
  
}

shinyApp(ui = ui, server = server)
