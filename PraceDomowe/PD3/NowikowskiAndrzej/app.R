#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Interactive zoom taken from: 
# https://shiny.rstudio.com/gallery/plot-interaction-zoom.html

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Interactive plotter"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        fileInput("csv_file",
                  "Please select a .csv file",
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
        ),
        selectInput("plot_type",
                  "Please select a plot type", choices=c("barplot", "scatterplot", "heatmap")
        ),
        uiOutput("plot_controls")
      ),
      
      mainPanel(
        plotOutput("plot",
                   dblclick = "dblclick",
                   brush = brushOpts(
                     id = "brush",
                     resetOnNew = TRUE
                   ))
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  csv <- reactive({
    validate(
      need(input$csv_file, "Please select .csv file")
    )
    as.data.frame(read.csv(input$csv_file$datapath))
  })
  
  observeEvent(input$dblclick, {
    brush <- input$brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$plot_controls <- renderUI({
    required_variables <- colnames(csv())
    optional_variables <- c('None', required_variables)
    if (input$plot_type == "scatterplot") {
      tagList(
        selectInput("x_variable", "X-axis", choices=required_variables),
        selectInput("y_variable", "Y-axis", choices=required_variables),
        selectInput("color", "Color", choices=optional_variables),
        selectInput("facet", "Facet", choices=optional_variables)
      )
    }
    else if (input$plot_type == "barplot") {
      tagList(
        selectInput("x_variable", "X-axis", choices=required_variables),
        selectInput("weight", "Weight", choices=optional_variables),
        selectInput("fill", "Fill", choices=optional_variables),
        selectInput("facet", "Facet", choices=optional_variables)
      )
    }
    else if (input$plot_type == "heatmap") {
      tagList(
        selectInput("x_variable", "X-axis", choices=required_variables),
        selectInput("y_variable", "Y-axis", choices=required_variables),
        selectInput("fill", "Fill", choices=optional_variables),
        selectInput("facet", "Facet", choices=optional_variables)
      )
    }
  })
  
  output$plot <- renderPlot({
    df <- csv()
    validate(
      need(input$plot_type, "Please select plot type")
    )
    plot <- ggplot(df)
    
    if (input$plot_type == 'scatterplot') {
      validate(
        need(input$x_variable, "Please select x variable"),
        need(input$y_variable, "Please select y variable")
      )
      aes <- aes_string(
        x=input$x_variable, 
        y=input$y_variable,
        color=input$color
      )
      
      if (input$color == 'None') {
        aes$colour <- NULL
      }
      plot <- plot + geom_point(aes) + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
    }
    

    if (input$plot_type == 'barplot') {
      validate(
        need(input$x_variable, "Please select x variable")
      )
      aes <- aes_string(
        x=input$x_variable,
        fill=input$fill,
        weight=input$weight
      )
      if (input$fill == 'None') {
        aes$fill <- NULL
      }
      if (input$weight == 'None') {
        aes$weight <- NULL
      }
      
      plot <- plot + geom_bar(aes)
    }
    
    if (input$plot_type == 'heatmap') {
      validate(
        need(input$x_variable, "Please select x variable"),
        need(input$y_variable, "Please select y variable")
      )
      aes <- aes_string(
        x=input$x_variable, 
        y=input$y_variable,
        fill=input$fill
      )
      if (input$fill == 'None') {
        aes$fill <- NULL
      }
      plot <- plot + geom_tile(aes)
    }
    
    if (input$facet != 'None') {
      plot <- plot + facet_wrap(c(input$facet))
    }
    
    plot
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

