# adapted from https://gist.github.com/garrettgman/6673251

library(shiny)
library(shinycssloaders)

f_names <- c("plotOutput", "tableOutput")

funs <- lapply(f_names, function(f_name) {
  function(...) {
    withSpinner(get(f_name, "package:shiny")(...))
  }
})

for (i in 1:length(funs)) {
  assign(paste0(f_names[i], "WithSpinner"), funs[[i]])
}


ui <- pageWithSidebar(
    headerPanel("Interactive Poisson"),
    sidebarPanel(
        sliderInput("lambda", "Choose lambda value for Poisson distribution", 
                     min = 0.01, max = 100, value = 1, step = 0.01)
    ),
    mainPanel(
        plotOutputWithSpinner("histogram"),
        tableOutputWithSpinner("table")
    )
)


server <- function(input, output) {
  
    poisdat <- reactive({ 
        rpois(1e6, input$lambda)
    })
    
    output[["histogram"]] <- renderPlot({
        hist(poisdat())
    })
    
    output[["table"]] <- renderTable({
        table(poisdat())
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
