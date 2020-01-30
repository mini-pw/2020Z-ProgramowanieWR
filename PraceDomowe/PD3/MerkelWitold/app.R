library(shiny)
library(plotly)
library(ggplot2)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("superhero"),
  titlePanel("PD3 - Witold Merkel"),
  
  
  sidebarLayout(
    
    sidebarPanel(
      fileInput(
        "data",
        "Select file .csv",
        accept = c(".csv")
      ),
      radioButtons("separator","Separator: ",choices = c(",", ";", ":"), selected=";",inline=TRUE),
      selectInput(
        "type",
        "Select type of plot",
        choices = c("Barchart",
                    "Heatmap",
                    "Violin plot",
                    "Density plot",
                    "Scatter plot")
      ),
      
      conditionalPanel( condition = "!input.type.localeCompare(\"Barchart\")",
                        checkboxInput("dodge", "Position dodge")),
      
      selectInput(
        "x",
        "Select what shall be on X axis",
        choices = c()
      ),
      selectInput(
        "y",
        "Select what shall be on Y axis",
        choices = c()
      ),
      selectInput(
        "color",
        "Select how the colors should work",
        choices = c()
      ),
      selectInput(
        "facet",
        "Specify the facet",
        choices = c()
      )
    ),
    
    mainPanel(
      plotlyOutput("plot")
    )
  )
)


server <- function(input, output, session) {
  
  observe({
    
    if(!is.null(input$data) & !is.null(input$separator)){
      
      data <- read.csv(input$data$datapath, sep = input$separator)
      
      updateSelectInput(session = session,
                        inputId = "x",
                        choices = colnames(data))
      
      updateSelectInput(session = session,
                        inputId = "y",
                        choices = colnames(data))
      
      updateSelectInput(session = session,
                        inputId = "color",
                        choices = colnames(data))
      
      updateSelectInput(session = session,
                        inputId = "facet",
                        choices = c("None",colnames(data)))
    }
  })
  
  output[["plot"]] <- renderPlotly({
    
    validate({
      need(input$data, "Please select the file")
      
    })
    
    data <- read.csv(input$data$datapath, sep = input$separator)
    plot_base <- ggplot(data, aes(x = data[[input$x]], 
                          y = data[[input$y]],
                          fill = data[[input$color]],
                          color = data[[input$color]])) +
      xlab(input$x) + 
      ylab(input$y) +
      facet_wrap(data[[input$facet]])
    
    if(input$type == "Heatmap"){
      plot <- plot_base + geom_tile()
    }
    else if(input$type == "Scatter plot"){
      plot <- plot_base + geom_point()
    }
    else if(input$type == "Barchart"){
      if(input$dodge){
        plot <- plot_base + geom_bar(stat = "identity", position = 'dodge') + ggtitle(input)
      } else {
        plot <- plot_base + geom_bar(stat = "identity")
      }
    }
    else if(input$type == "Density plot"){
      plot <- plot_base + geom_density2d()
    }
    else if(input$type == "Violin plot"){
      plot <- plot_base + 
        geom_violin()
    }
    
    ggplotly(plot)
    
  })
  
  outputOptions(output, "plot", suspendWhenHidden = FALSE)  
}

shinyApp(ui = ui, server = server)