library(shiny)
library(ggplot2)
library(plotly)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("flatly"),
    titlePanel("Universal plotter"),
    
    sidebarLayout(
        
        sidebarPanel(
            
            fileInput(
                "data",
                "Upload data"
            ),
            selectInput(
                "type",
                "Select graph type",
                choices = c("Bar plot",
                            "Scatter plot",
                            "Heatmap",
                            "Line plot")
            ),
            selectInput("x", "X axis", choices = c()),
            selectInput("y", "Y axis", choices = c()),
            selectInput("color", "Color", choices = c()),
            selectInput("line", "Line", choices = c()),
            selectInput("facetRows", "Facet Rows", choices = c())
        ),
        
        mainPanel(
            plotlyOutput("plot")
        )
    )
)
    
server <- function(input, output, session) {
    
    output$plot <- renderPlotly({
        
        validate(
            need(input$data, 'Please, upload data')
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
            inputId = "color",
            selected = input$color,
            choices = colnames(data)
        )
        
        updateSelectInput(
            session = session,
            inputId = "line",
            selected = input$line,
            choices = colnames(data)
        )
        
        updateSelectInput(
            session = session,
            inputId = "facetRows",
            selected = input$facetRows,
            choices = c(colnames(data), "None")
        )
        
        validate(
            need(input$x, 'Please select X axis'),
            need(input$y, 'Please select Y axis'),
            need(input$color, 'Please select color'),
            need(input$line, 'Please select line type'),
            need(input$facetRows, "Please select facet rows")
        )
        
        if (input$type == "Bar plot") {
            
            p <- ggplot(data, aes(x = data[[input$x]], fill = input$color)) +
                geom_bar(stat = "count")
            
        } else if ( input$type == "Scatter plot") {
            
            p <- ggplot(data, aes(x = data[[input$x]], y = data[[input$y]], fill = data[[input$color]])) + 
                geom_point()
            
        } else if (input$type == "Line plot") {
            
            p <- ggplot(data, aes(x = data[[input$x]], y = data[[input$y]], fill = data[[input$color]])) + 
                geom_line(aes(linetype = data[[input$line]]))
            
        } else {
            
            p <- ggplot(data, aes(x = data[[input$x]], y = data[[input$y]], fill = data[[input$color]])) + 
                geom_tile()  
        }
        
        
        if (input$facetRows != "None") {
            p <- p + facet_grid(rows = input$facetRows)
        } 
        
        ggplotly(p)
    })
}

shinyApp(ui = ui, server = server)
