library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

updatable_selects <- c()

my_selector <- function(id, name){
  updatable_selects <<- c(id, updatable_selects)
  updatable_selects <<- unique(updatable_selects)
  selectInput(id, name, choices=c())
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      tags$hr(),
      checkboxInput("header", "Header", TRUE),
      selectInput("plot_type", "Plot type", list("scatter", "boxplot", "heatmap")),
      checkboxInput('facet_wrap_enabled', 'Facet Wrap'),
      my_selector("facet_wrap", "Facet Wrap"),
      conditionalPanel(
        condition = "input.plot_type == 'scatter'",
        "scatter input",
        my_selector("scatter_x", "X"),
        my_selector("scatter_y", "Y"),
        my_selector("scatter_color", "Color"),
        selectInput('scatter_shape', 'Shape', 0:25)
      ),
      conditionalPanel(
        condition = "input.plot_type == 'boxplot'",
        "boxplot input",
        my_selector("boxplot_x", "X"),
        my_selector("boxplot_y", "Y"),
        my_selector("boxplot_color", "Color"),
      ),
      conditionalPanel(
        condition = "input.plot_type == 'heatmap'",
        "heatmap input",
        my_selector("heatmap_x", "X"),
        my_selector("heatmap_y", "Y"),
        my_selector("heatmap_fill", "Fill"),
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotlyOutput("plot")),
        tabPanel("Contents", tableOutput("contents"))
      )
    )
  )
)
  
server <- function(session, input, output) {
  
  data <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    df <- read.csv(inFile$datapath, header = input$header)
    print(updatable_selects)
    for(selector_id in updatable_selects){
      updateSelectInput(session, selector_id, choices=colnames(df))  
    }
    df
  })
  
  output$contents <- renderTable({
    data()
  })
  
  plot_reactive <- reactive({
    if(is.null(data())){
      return(NULL)
    }
    if( input$plot_type == "scatter"){
      parsed_aes <- aes_string(x=input[["scatter_x"]], 
                               y=input[["scatter_y"]], 
                               color=input[["scatter_color"]])
      p <- ggplot(data(), parsed_aes) + geom_point(shape=input[['scatter_shape']])
    } else if (input$plot_type == "heatmap"){
      parsed_aes <- aes_string(x=input[["heatmap_x"]], 
                               y=input[["heatmap_y"]], 
                               fill=input[["heatmap_fill"]])
      p <- ggplot(data(), parsed_aes) + geom_tile()
    } else if (input$plot_type == "boxplot"){
      parsed_aes <- aes_string(x=input[["boxplot_x"]], 
                               y=input[["boxplot_y"]], 
                               color=input[["boxplot_color"]])
      p <- ggplot(data(), parsed_aes) + geom_boxplot()
    }
    if( input[['facet_wrap_enabled']]){
      formula <-paste0(input[['facet_wrap']], "~.")
      print(formula)
      p + facet_wrap(formula)  
    } else {
      p
    }
  })
  
  output$plot <- renderPlotly({
    # p <- ggplot(data(), aes_reactive()) + geom_point()
    p <- plot_reactive()
    if(is.null(p) == FALSE){
      ggplotly(plot_reactive())
    }
  })
  
}

shinyApp(ui, server)
