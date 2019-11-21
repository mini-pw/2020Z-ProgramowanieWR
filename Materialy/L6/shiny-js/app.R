# adapted from https://gist.github.com/garrettgman/6673251

library(shiny)
library(shinycssloaders)
library(shinyhelper)

f_names <- c("numericInput", "selectInput", "sliderInput")
l <- lapply(f_names, function(ith_f_name) 
    function(inputId, ...) 
        helper(getFromNamespace(ith_f_name, ns = "shiny")(inputId = inputId, ...),  
               content = inputId, type = "markdown")
    )

for(i in 1L:length(f_names)) {
    assign(x = paste0(f_names[i], "Helper"), value = l[[i]])
}


ui <- pageWithSidebar(
    headerPanel("Interactive Histogram"),
    sidebarPanel(
        numericInputHelper("n", "Generate this many points", 
                     min = 1, value = 10e6),
        selectInputHelper("family", "From this family",
                    choices = c("Normal",
                                "Uniform",
                                "Exponential"),
                    selected = "normal"),
        sliderInputHelper("bins", "number of bins", 
                    min = 1, max = 100, value = 50)
    ),
    mainPanel(
        withSpinner(helper(plotOutput("histogram"), content = "plot", type = "markdown"))
    )
)


server <- function(input, output) {
    
    observe_helpers(withMathJax = TRUE, help_dir = "markdowny_pomocowe")
    
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
