# adapted from https://gist.github.com/garrettgman/6673251

library(shiny)
library(shinycssloaders)
library(shinyhelper)

shinyFunctionList = c("plotOutput", "tableOutput")

functionList = lapply(shinyFunctionList,
                      function(f_name) {
                          function(inputId, ...) {
                              withSpinner(getFromNamespace(f_name, ns = "shiny")(outputId = inputId, ...))
                          }
                      })

for (i in 1:length(shinyFunctionList)) {
    nameWithHelper = paste0(shinyFunctionList[i], "Spinner")
    print(nameWithHelper)
    assign(nameWithHelper, functionList[[i]])
}

ui <- pageWithSidebar(
    headerPanel("Interactive Histogram"),
    sidebarPanel(
        numericInput(
            "lambda",
            "Generate this many points",
            min = 1,
            max = 100,
            value = 5
        )
    ),
    mainPanel(plotOutputSpinner("histogram"), 
              tableOutputSpinner("table"))
)

server <- function(input, output) {
    observe_helpers(help_dir = "help_mds")
    
    lambda = reactive({
        input[["lambda"]]
    })
    
    output[["histogram"]] <- renderPlot({
        print(lambda())
        d = density(rpois(1000000, lambda()))
        plot(d)
    })
    
    output[["table"]] <- renderTable(table(rpois(1000000, lambda())))
}

# Run the application
shinyApp(ui = ui, server = server)
