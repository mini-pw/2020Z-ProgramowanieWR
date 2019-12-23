#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
         plotOutput("plot")
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
  })
  
  output$plot <- renderPlot({
    df <- csv()

    aes <- aes_string(
      x=input$x_variable, 
      y=input$y_variable,
      color=input$color
    )
    if (input$color == 'None') {
      aes$colour <- NULL
    }
  
    plot <- ggplot(df, aes)
    
    if (input$plot_type == 'scatterplot') {
      plot <- plot + geom_point()
    }
    
    if (input$facet != 'None') {
      plot <- plot + facet_wrap(c(input$facet))
    }
    plot
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

