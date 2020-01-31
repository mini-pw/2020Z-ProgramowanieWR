library(shiny)
library(plotly)

ui <- fluidPage(
  
  titlePanel("Data visualizator"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
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
      
      tags$hr(),
      
      selectInput(inputId = "plot_type", label = "Select plot type", 
                  choices = c("Scatter plot", "Bar chart", "Heatmap")
      ),
      
      tags$hr(),
      
      selectInput(inputId = "x_axis", label = "Select x axis", 
                  choices = c()
      ),
      
      selectInput(inputId = "y_axis", label = "Select y axis", 
                  choices = c()
      ),
      
      selectInput(inputId = "color", label = "Select color", 
                  choices = c()
      ),
      
      selectInput(inputId = "facet", label = "Select facet", 
                  choices = c()
      )
  
    ),
    
    mainPanel(
      
      plotlyOutput("plot")
      
    )
    
  )
)

server <- function(input, output, session) {
  
  dat <- reactive({
    
    req(input$file1)
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
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

  observe({
    columns <- colnames(dat())
    columns_exp <- c(columns, '-')
    
    updateSelectInput(session, "x_axis",
                      choices = columns,
                      selected = columns[[1]]
    )
    
    updateSelectInput(session, "y_axis",
                      choices = columns_exp,
                      selected = columns_exp[[2]]
    )
    
    updateSelectInput(session, "color",
                      choices = columns_exp,
                      selected = columns_exp[[length(columns_exp)]]
    )
    
    updateSelectInput(session, "facet",
                      choices = columns_exp,
                      selected = columns_exp[[length(columns_exp)]]
    )
    
  })
  
  add_param <- function(plot_df, paramName) {
    if (input[[paramName]] != '-') {
      param <- dat()[,input[[paramName]]]
    }
    else {
      param <- as.factor(rep(1, nrow(plot_df)))
    }
    new_columns <- c(colnames(plot_df), paramName)
    plot_df <- cbind(plot_df, param)
    colnames(plot_df) <- new_columns
    plot_df
  }
  
  observeEvent({
    input$plot_type
    input$x_axis
    input$y_axis
    input$color
    input$facet
  },{

    plot_df <- data.frame(dat()[,input$x_axis])
    
    colnames(plot_df) <- c("x_axis")
    
    plot_df <- add_param(plot_df, "y_axis")
    
    plot_df <- add_param(plot_df, "color")

    plot_df <- add_param(plot_df, "facet")
    
    output$plot <- renderPlotly({
      p <- ggplot(plot_df, aes(x=x_axis)) + labs(x=input$x_axis, col=input$color, fill=input$color)
      
      if (input$plot_type == 'Scatter plot') {
        p <- p + geom_point(aes(y=y_axis, col=color)) + labs(y=input$y_axis)
      }
      else if (input$plot_type == 'Bar chart') {
        if (input$y_axis == '-') {
          p <- p + geom_bar(aes(fill=color))
        }
        else {
          p <- p + geom_bar(aes(y=y_axis, fill=color), stat='identity') + labs(y=input$y_axis)
        }
      }
      else if (input$plot_type == 'Heatmap') {
        p <- p + geom_tile(aes(y=y_axis, fill=color)) + labs(y=input$y_axis)
      }
      
      if (input$color == '-') {
        p <- p + theme(legend.position = "none")
      }
      
      if (input$facet != '-') {
        p <- p + facet_grid(facet ~ .)
      }

      ggplotly(p)
    })
  })
  
}

shinyApp(ui, server)