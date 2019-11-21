# adapted from https://gist.github.com/garrettgman/6673251

library(shiny)
library(shinycssloaders)

ui <- pageWithSidebar(
    headerPanel("Interactive Histogram"),
    sidebarPanel(
        numericInput("n", "Generate this many points", 
                     min = 1, value = 10e6),
        selectInput("family", "From this family",
                    choices = c("Normal",
                                "Uniform",
                                "Exponential"),
                    selected = "normal"),
        sliderInput("bins", "number of bins", 
                    min = 1, max = 100, value = 50)
    ),
    mainPanel(
        withSpinner(plotOutput("histogram"))
    )
)


server <- function(input, output) {
    
    data <- reactive({ 
        FUN <- switch(input[["family"]],
                      "Normal" = rnorm,
                      "Uniform" = runif,
                      "Exponential" = rexp)
        FUN(input[["n"]])
    })
    
    output[["histogram"]] <- renderPlot({
        hist(data(), breaks = input$bins)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
