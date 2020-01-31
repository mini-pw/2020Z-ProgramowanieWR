library(shiny)

ui <- fluidPage(
  titlePanel("DrawR!"),
  tabsetPanel(
    tabPanel("Data",
      h3("Select data to visualize"),
      fileInput(inputId = "data", 
                label = "Input data", 
                accept = ".csv", 
                placeholder = "some_data.csv"),
      textOutput("data_info"),
      tableOutput("data_table")
    ),
    tabPanel("Plot type",
      h3("Select type of the plot"),
      selectInput(inputId = "plot_type",
                  label = "Choose wisely!",
                  choices = c("barchart",
                              "heatmap",
                              "scatterplot"))
    ),
    tabPanel("Variables",
      h3("Select bindings for variables"),
      uiOutput("variables_bindings")
    ),
    tabPanel("Faceting",
      h3("Select variables by which plot should be faceted"),
      uiOutput("faceting_bindings")
    ),
    tabPanel("Plot!",
      h3("Here you have - plot from your dreams!"),
      plotOutput(outputId = "rendered_plot"))
  )
)