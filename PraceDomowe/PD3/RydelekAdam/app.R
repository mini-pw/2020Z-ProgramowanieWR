library(shiny)
library(ggplot2)
library(plotly)

ui <- fluidPage(titlePanel("PD3"),
                
                sidebarLayout(
                  sidebarPanel(
                    fileInput(
                      "data",
                      "Upload your data"
                    ),
                    selectInput(
                      "type",
                      "Select graph type",
                      choices = c("Heatmap",
                                  "Barchart",
                                  "Scatterplot")
                    ),
                    selectInput("x", "X", choices = c()),
                    selectInput("y", "Y", choices = c()),
                    selectInput("fill", "Fill", choices = c()),
                    selectInput("facet", "Facet rows", choices = c())
                  ),
                  
                  mainPanel(
                    plotlyOutput("plot")
                  )
                )
)

server <- function(input, output, session) {
  
  output$plot <- renderPlotly({
    
    validate(
      need(input$data, 'Upload data')
    )
    
    data <- read.csv(input$data$datapath)
    updateSelectInput(
      session = session,
      inputId = "x",
      selected = input$x,
      choices = colnames(data)
    )
    updateSelectInput(
      session = session,
      inputId = "y",
      selected = input$y,
      choices = colnames(data)
    )
    updateSelectInput(
      session = session,
      inputId = "fill",
      selected = input$fill,
      choices = colnames(data)
    )
    updateSelectInput(
      session = session,
      inputId = "facet",
      selected = input$facet,
      choices = c(colnames(data), "None")
    )
    
    validate(
      need(input$x, 'Select X axis'),
      need(input$y, 'Select Y axis'),
      need(input$fill, 'Select fill'),
      need(input$facet, 'Select facet rows')
    )
    
    if (input$type == "Barchart") {
      
      plot <- ggplot(data, aes(x = data[[input$x]], fill = data[[input$fill]])) +
        theme_bw() + xlab(input$x) + ylab(input$y) + labs(fill = input$fill) + geom_bar(stat = "count")
      
    } else if ( input$type == "Scatterplot") {
      
      plot <- ggplot(data, aes(x = data[[input$x]], y = data[[input$y]], fill = data[[input$fill]])) + 
        theme_bw() + xlab(input$x) + ylab(input$y) + labs(fill = input$fill) + geom_point()
      
    } else {
      
      plot <- ggplot(data, aes(x = data[[input$x]], y = data[[input$y]], fill = data[[input$fill]])) + 
        theme_bw() + xlab(input$x) + ylab(input$y) + labs(fill = input$fill) + geom_tile()  
    }
    
    
    if (input$facet != "None") {
      plot <- plot + facet_grid(rows = input$facet)
    } 
    
    ggplotly(plot)
  })
}

shinyApp(ui = ui, server = server)
