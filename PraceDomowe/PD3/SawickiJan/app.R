library(shiny)
library(ggplot2)
library(plotly)

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
    plotlyOutput("graph")
)

server <- function(input, output, session) {
    dataset = data.frame()
    
    output$graph = renderPlotly({
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
                plot = ggplot(data = dataset,
                              aes(
                                  x = dataset[[input[["firstColumn"]]]],
                                  fill = dataset[[input[["thirdColumn"]]]],
                                  col = dataset[[input[["fourthColumn"]]]],
                                  text = paste0(
                                      unique(input[["firstColumn"]]),
                                      ": ",
                                      unique(dataset[[input[["firstColumn"]]]]),
                                      "\r\n",
                                      "count",
                                      ": ",
                                      ..count..
                                  )
                              )) +
                    xlab(input[["firstColumn"]]) +
                    ylab(input[["secondColumn"]]) +
                    guides(fill = guide_legend(title = input[["thirdColumn"]]),
                           col = guide_legend(title = input[["fourthColumn"]])) +
                    geom_bar(stat = "count",
                             mapping = aes(linetype = dataset[[input[["fourthColumn"]]]]))
            }
            else if (input[["graphType"]] == "Scatter") {
                plot = ggplot(data = dataset,
                              aes(
                                  x = dataset[[input[["firstColumn"]]]],
                                  y = dataset[[input[["secondColumn"]]]],
                                  fill = dataset[[input[["thirdColumn"]]]],
                                  text = paste0(
                                      input[["firstColumn"]],
                                      ": ",
                                      dataset[[input[["firstColumn"]]]],
                                      "\r\n",
                                      input[["secondColumn"]],
                                      ": ",
                                      dataset[[input[["secondColumn"]]]],
                                      "\r\n",
                                      input[["thirdColumn"]],
                                      ": ",
                                      dataset[[input[["thirdColumn"]]]]
                                  )
                              )) +
                    xlab(input[["firstColumn"]]) +
                    ylab(input[["secondColumn"]]) +
                    guides(fill = guide_legend(title = input[["thirdColumn"]]),
                           col = guide_legend(title = input[["fourthColumn"]])) +
                    geom_point()
            }
            else if (input[["graphType"]] == "Heatmap") {
                plot = ggplot(data = dataset,
                              aes(
                                  x = dataset[[input[["firstColumn"]]]],
                                  y = dataset[[input[["secondColumn"]]]],
                                  fill = dataset[[input[["thirdColumn"]]]],
                                  text = paste0(
                                      input[["firstColumn"]],
                                      ": ",
                                      dataset[[input[["firstColumn"]]]],
                                      "\r\n",
                                      input[["secondColumn"]],
                                      ": ",
                                      dataset[[input[["secondColumn"]]]],
                                      "\r\n",
                                      input[["thirdColumn"]],
                                      ": ",
                                      dataset[[input[["thirdColumn"]]]]
                                  )
                              )) +
                    xlab(input[["firstColumn"]]) +
                    ylab(input[["secondColumn"]]) +
                    guides(fill = guide_legend(title = input[["thirdColumn"]]),
                           col = guide_legend(title = input[["fourthColumn"]])) +
                    geom_tile()
            }
            
            print(ggplotly(p = plot, tooltip = "text"))
        }
    })
}

shinyApp(ui = ui, server = server)