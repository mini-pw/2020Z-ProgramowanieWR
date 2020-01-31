library(shiny)
library(ggplot2)
library(plotly)
library(shinythemes)


ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("CSV to plot"),
  tabsetPanel(id = "tabs",
              tabPanel("Upload the data here",
                       sidebarLayout(
                         sidebarPanel(
                           fileInput(
                             "data",
                             "Choose CSV File"
                           ),
                           tags$hr(),
                           checkboxInput("header", "Header", TRUE),
                           radioButtons("sep", "Separator",
                                        choices = c(Comma = ",",
                                                    Semicolon = ";",
                                                    Tab = "\t"),
                                        selected = ","),
                           radioButtons("quote", "Quote",
                                        choices = c(None = "",
                                                    "Double Quote" = '"',
                                                    "Single Quote" = "'"),
                                        selected = '"'),
                           tags$hr()
                         ),
                         mainPanel(
                           DT::dataTableOutput("contents")
                         )
                       )
              ),
              tabPanel("Plot the data here",
                       sidebarLayout(sidebarPanel(selectInput(
                         "type",
                         "Select plot type",
                         choices = c("Bar plot",
                                     "Scatter plot",
                                     "Heatmap",
                                     "Line plot",
                                     "Density plot",
                                     "Violin plot"
                         ),
                         selected = "Bar plot"
                       ),
                       selectInput("x", "X axis", choices = c()),
                       selectInput("y", "Y axis", choices = c()),
                       selectInput("color", "Color", choices = c()),
                       conditionalPanel( condition = "!input.type.localeCompare(\"Scatter plot\") || !input.type.localeCompare(\"Line plot\") ",
                                         selectInput("shape", "Shape", choices = 0:25L, selected = 19)
                       ),
                       
                       selectInput("facetRows", "Facet by factor", choices = c(), selected  = "None"),
                       
                       
                       conditionalPanel( condition = "!input.type.localeCompare(\"Density plot\") || !input.type.localeCompare(\"Line plot\")",
                                         selectInput(
                                           "line",
                                           "Select linetype",
                                           choices = c( "solid", "dashed", "dotted", "dotdash"),
                                           selected = "solid"
                                         ))
                       ),
                       mainPanel(
                         plotlyOutput("plot")
                       )
                       )
              )
  )
)

server <- function(input, output, session) {
  
  observe({
    check <- input$data
    if(!is.null(input$data)){
      data <- read.csv(input$data$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      
      updateSelectInput(
        session = session,
        inputId = "x",
        choices = colnames(data)
      )
      updateSelectInput(
        session = session,
        inputId = "y",
        choices = colnames(data)
      )
      
      updateSelectInput(
        session = session,
        inputId = "color",
        choices = colnames(data)
      )
      updateSelectInput(
        session = session,
        inputId = "facetRows",
        selected  = "None",
        choices = c(colnames(Filter(is.factor, data)), "None")
      )
    }
  })
  output$contents <- DT::renderDataTable({
    validate(
      need(input$data, 'Upload your data')
    )
    req(input$data)
    tryCatch(
      {
        df <- read.csv(input$data$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    df
  })
  
  output$plot <- renderPlotly({
    data <- read.csv(input$data$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
    validate(
      need(input$data, 'Upload your data')
    )
    validate(
      need(input$x, "Wait a second"),
      need(input$y, ""),
      need(input$color, ""),
      need(input$line, ""),
      need(input$facetRows,"")
    )
    
    p <- ggplot(data, aes(x = data[[input$x]], 
                          y = data[[input$y]],
                          fill = data[[input$color]],
                          color = data[[input$color]])) +
      xlab(input$x) + 
      ylab(input$y) +
      facet_wrap(data[[input$facetRows]])
    
    if(input$type == "Heatmap"){
      ply <- p + geom_tile()
    }
    else if(input$type == "Scatter plot"){
      ply <- p + geom_point(shape = input$shape)
    }
    else if(input$type == "Bar plot"){
      
      ply <- p + geom_bar(stat = "identity")
      
    }
    else if(input$type == "Density plot"){
      ply <- p + geom_density2d(linetype = input$line)
    }
    else if(input$type == "Violin plot"){
      ply <-p + 
        geom_violin()
    }
    else if(input$type == "Line plot"){
      ply <- p + geom_line(linetype = input$line)
    }
    
    ply = ply +
      xlab(input[["x"]]) +
      ylab(input[["y"]]) +
      guides(fill = guide_legend(title = input[["color"]]),
             col = guide_legend(title = input[["line"]]))
    
    ggplotly(ply)
  })
}

shinyApp(ui = ui, server = server) 