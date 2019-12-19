library(shiny)
library(ggplot2)
library(ggbeeswarm)
library(plotly)

ui <- basicPage(
  selectInput(
    "graphType",
    "Make a graph of type",
    choices = c("Bar",
                "Scatter",
                "Heatmap",
                "Jitter",
                "Beeswarm"),
    selected = "Bar"
  ),
  fileInput(
    "mainFile",
    "from file",
    accept = c("text/csv",
               "text/comma-separated-values,text/plain",
               ".csv")
  ),
  selectInput("x", "put on a x axis", choices = c()),
  conditionalPanel(condition = "input.graphType != \"Bar\"",
                   selectInput("y", "put on a y axis", choices = c())),
  selectInput("fill", "fill it with (factors only)", choices = c()),
  conditionalPanel(
    condition = "input.graphType == \"Bar\"",
    selectInput("line", "to draw line use (factors only)", choices = c())
  ),
  selectInput("facet", "facet everything by (factors only)", choices = c()),
  plotlyOutput("graph")
)

server <- function(input, output, session) {
  dataset = data.frame()
  
  updateSelectInputs = function(session, inputIds, dataset) {
    for (inputId in inputIds) {
      if (inputId == "x" || inputId == "y") {
        updateSelectInput(
          session = session,
          inputId = inputId,
          selected = input[[inputId]],
          choices = colnames(dataset)
        )
      }
      else{
        updateSelectInput(
          session = session,
          inputId = inputId,
          selected = input[[inputId]],
          choices = colnames(Filter(is.factor, dataset))
        )
      }
    }
  }
  
  output$graph = renderPlotly({
    if (!is.null(input[["mainFile"]])) {
      plot = NULL
      
      filename = input[["mainFile"]]$datapath
      dataset = read.csv(filename)
      
      inputIds = c("x", "y", "fill", "line", "facet")
      
      updateSelectInputs(session, inputIds, dataset)
      
      validate(need(input[["x"]], "Please select data for x axis"))
      
      if (input[["graphType"]] != "Bar") {
        validate(need(input[["y"]], "Please select data for y axis"))
      }
      
      x = dataset[[input[["x"]]]]
      y = dataset[[input[["y"]]]]
      fill = dataset[[input[["fill"]]]]
      # https://github.com/ropensci/plotly/issues/1562
      if (input[["graphType"]] == "Heatmap") {
        if (is.null(dataset[[input[["fill"]]]]))
          fill = rep("data", length(x))
      }
      line =  dataset[[input[["line"]]]]
      
      
      switch(
        input[["graphType"]],
        Bar = {
          plot = ggplot(data = dataset,
                        aes(
                          x = x,
                          fill = fill,
                          col = line
                        )) +
            geom_bar(stat = "count",
                     mapping = aes(linetype = line))
        },
        Heatmap = {
          plot = ggplot(data = dataset,
                        aes(
                          x = x,
                          y = y,
                          fill = fill
                        )) +
            geom_tile()
        },
        Scatter = {
          plot = ggplot(data = dataset,
                        aes(
                          x = x,
                          y = y,
                          fill = fill
                        )) +
            geom_point()
        },
        Jitter = {
          plot = ggplot(data = dataset,
                        aes(
                          x = x,
                          y = y,
                          fill = fill,
                          colour = fill
                        )) +
            geom_jitter()
        },
        Beeswarm = {
          plot = ggplot(data = dataset,
                        aes(
                          x = x,
                          y = y,
                          fill = fill,
                          colour = fill
                        )) +
            geom_quasirandom(grouponX = TRUE)
        },
        {
          
        }
      )
      
      plot = plot +
        xlab(input[["x"]]) +
        ylab(input[["y"]]) +
        guides(fill = guide_legend(title = input[["fill"]]),
               col = guide_legend(title = input[["line"]]))
      
      if (input[["facet"]] != "") {
        plot = plot +
          facet_wrap(~ dataset[[input[["facet"]]]])
      }
      
      print(ggplotly(p = plot))
    }
  })
}

shinyApp(ui = ui, server = server)