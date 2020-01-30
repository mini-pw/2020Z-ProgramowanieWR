library(shiny)
library(ggplot2)
library(plotly)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("cyborg"),
                titlePanel("PD3"),
                sidebarLayout(
                  sidebarPanel(
                    fileInput("data","Upload plot data"),
                    selectInput(
                      "type",
                      "Type of the plot",
                      choices = c("Scatter plot", "Line plot", "Bar plot", "Heatmap")
                    ),
                    selectInput("x", "X aesthetic", choices = NULL),
                    selectInput("y", "Y aesthetic", choices = NULL),
                    selectInput("color", "Color aesthetic", choices = NULL),
                    selectInput("line", "Line aesthetic", choices = NULL),
                    selectInput("facet", "Facet", choices = NULL)
                  ),
                  mainPanel(
                    plotlyOutput("plot")
                  )
                )
)

server <- function(input, output, session) {
  
  
  observe({
    validate(need(input$data, 'Please upload data'))
    data <- read.csv(input$data$datapath)
    lapply(list("x", "y", "color", "line", "facet"), function(x){
      updateSelectInput(
        session = session,
        inputId = x,
        selected = input[[x]],
        choices = c(colnames(data), "NULL")
      )
    })
  })
  
  makePlane <- reactive({
    
    validate(need(input$data, 'Please upload data'))
    data <- read.csv(input$data$datapath)
    validate(
      need(input$x, "Please select X aesthetic (Can be NULL)"),
      need(input$y, "Please select Y aesthetic (Can be NULL)"),
      need(input$color, "Please select aesthetic (Can be NULL)"),
      need(input$line, "Please select line aesthetic (Can be NULL)"),
      need(input$facet, "Please select facet (Can be NULL)")
    )
    
    if (input$x == "NULL") {
      x <- NULL
    } else {
      x <- input$x 
    }
    
    if (input$y == "NULL") {
      y <- NULL
    } else {
      y <- input$y 
    }
    
    if (input$color == "NULL") {
      color <- NULL
    } else {
      color <- input$color 
    }
    
    if (input$line == "NULL") {
      line <- NULL
    } else {
      line <- input$line 
    }
    
    ggplot(data = data, aes_string(x = x, y = y, fill = color, linetype = line))
  })
  
  output$plot <- renderPlotly({
    
    p <- makePlane()
    
    if (input$type == "Scatter plot") {
      
      p <- p +
        geom_point()
        
      
    } else if (input$type == "Line plot") {
      
      p <- p + 
        geom_line()
      
    } else if (input$type == "Bar plot") {
      
      p <- p + 
        geom_bar(stat = "identity")
      
    } else if ((input$type == "Heatmap")){
      
      p <- p + 
        geom_tile()  
    } else {
      stop(safeError())
    }
    
    
    if (input$facet!= "NULL") {
      p <- p + 
        facet_grid(rows = input$facet)
    } 
    
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)