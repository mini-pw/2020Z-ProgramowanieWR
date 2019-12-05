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
  selectInput("x", "what", choices = c()),
  selectInput("y", "of what", choices = c()),
  selectInput("fill", "fill", choices = c()),
  selectInput("line", "line", choices = c()),
  selectInput("facet", "facet", choices = c()),
  plotlyOutput("graph")
)

server <- function(input, output, session) {
  dataset = data.frame()
  
  updateSelectInputs = function(session, inputIds, dataset) {
    for (inputId in inputIds) {
      updateSelectInput(
        session = session,
        inputId = inputId,
        selected = input[[inputId]],
        choices = colnames(dataset)
      )
    }
  }
  
  output$graph = renderPlotly({
    if (!is.null(input[["mainFile"]])) {
      filename = input[["mainFile"]]$datapath
      dataset = read.csv(filename)
      
      inputIds = c("x", "y", "fill", "line", "facet")
      
      updateSelectInputs(session, inputIds, dataset)
      
      x = dataset[[input[["x"]]]]
      y = dataset[[input[["y"]]]]
      fill =  dataset[[input[["fill"]]]]
      line =  dataset[[input[["line"]]]]
      
      if (input[["graphType"]] == "Bar") {
        plot = ggplot(data = dataset,
                      aes(
                        x = x,
                        fill = fill,
                        col = line,
                        text = paste0(
                          unique(input[["x"]]),
                          ": ",
                          unique(x),
                          "\r\n",
                          "count",
                          ": ",
                          ..count..
                        )
                      )) +
          xlab(input[["x"]]) +
          ylab(input[["y"]]) +
          guides(fill = guide_legend(title = input[["fill"]]),
                 col = guide_legend(title = input[["line"]])) +
          geom_bar(stat = "count",
                   mapping = aes(linetype = line)) +
          facet_wrap( ~ dataset[[input[["facet"]]]])
      }
      else if (input[["graphType"]] == "Scatter") {
        plot = ggplot(data = dataset,
                      aes(
                        x = x,
                        y = y,
                        fill = fill,
                        text = paste0(
                          input[["x"]],
                          ": ",
                          x,
                          "\r\n",
                          input[["y"]],
                          ": ",
                          y,
                          "\r\n",
                          input[["fill"]],
                          ": ",
                          fill
                        )
                      )) +
          xlab(input[["x"]]) +
          ylab(input[["y"]]) +
          guides(fill = guide_legend(title = input[["fill"]]),
                 col = guide_legend(title = input[["line"]])) +
          geom_point() +
          facet_wrap( ~ facet)
      }
      else if (input[["graphType"]] == "Heatmap") {
        plot = ggplot(data = dataset,
                      aes(
                        x = x,
                        y = y,
                        fill = fill,
                        text = paste0(
                          input[["x"]],
                          ": ",
                          x,
                          "\r\n",
                          input[["y"]],
                          ": ",
                          y,
                          "\r\n",
                          input[["fill"]],
                          ": ",
                          fill
                        )
                      )) +
          xlab(input[["x"]]) +
          ylab(input[["y"]]) +
          guides(fill = guide_legend(title = input[["fill"]]),
                 col = guide_legend(title = input[["line"]])) +
          geom_tile() +
          facet_wrap( ~ facet)
      }
      
      print(ggplotly(p = plot, tooltip = "text"))
    }
  })
}

shinyApp(ui = ui, server = server)