library(shiny)
library(ggplot2)

ui <- basicPage(
    fileInput(
        "mainFile",
        "Choose CSV File",
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
    ),
    selectInput(
        "graphType",
        "From this family",
        choices = c("Bar",
                    "Scatter",
                    "Heatmap"),
        selected = "Bar"
    ),
    selectInput("firstColumn", "what", choices = c()),
    selectInput("secondColumn", "of what", choices = c()),
    selectInput("thirdColumn", "color", choices = c()),
    selectInput("fourthColumn", "line", choices = c()),
    plotOutput("graph")
)

server <- function(input, output, session) {
    dataset = data.frame()
    
    output$graph = renderPlot({
        if (!is.null(input[["mainFile"]])) {
            filename = input[["mainFile"]]$datapath
            dataset = read.csv(filename)
            
            updateSelectInput(
                session = session,
                inputId = "firstColumn",
                selected = input[["firstColumn"]],
                choices = colnames(dataset)
            )
            updateSelectInput(
                session = session,
                inputId = "secondColumn",
                selected = input[["secondColumn"]],
                choices = colnames(dataset)
            )
            updateSelectInput(
                session = session,
                inputId = "thirdColumn",
                selected = input[["thirdColumn"]],
                choices = colnames(dataset)
            )
            updateSelectInput(
                session = session,
                inputId = "fourthColumn",
                selected = input[["fourthColumn"]],
                choices = colnames(dataset)
            )
            
            if (input[["graphType"]] == "Bar") {
                ggplot(data = dataset,
                       aes(dataset[[input[["firstColumn"]]]],
                           fill = dataset[[input[["thirdColumn"]]]], col = dataset[[input[["fourthColumn"]]]])) +
                    geom_bar(stat = "count",
                             mapping = aes(linetype = dataset[[input[["fourthColumn"]]]]))
            }
            else if (input[["graphType"]] == "Scatter") {
                ggplot(data = dataset,
                       aes(
                           x = dataset[[input[["firstColumn"]]]],
                           y = dataset[[input[["secondColumn"]]]],
                           fill = dataset[[input[["thirdColumn"]]]]
                       )) +
                    geom_point()
            }
            else if (input[["graphType"]] == "Heatmap") {
                ggplot(data = dataset,
                       aes(
                           x = dataset[[input[["firstColumn"]]]],
                           y = dataset[[input[["secondColumn"]]]],
                           fill = dataset[[input[["thirdColumn"]]]]
                       )) +
                    geom_tile()
            }
        }
    })
}

shinyApp(ui = ui, server = server)