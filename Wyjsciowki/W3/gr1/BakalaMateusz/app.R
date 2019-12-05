# adapted from https://gist.github.com/garrettgman/6673251

library(shiny)
library(shinycssloaders)

add_spinner <- function(inputs) {
  list_of_funs <- lapply(inputs, function(fun_name) {
    list(
      fun = function(outputId, ...) {
        shinycssloaders::withSpinner(getFromNamespace(fun_name, ns = "shiny")(outputId = outputId, ...))
      },
      name = paste0(fun_name, "Spinner")
    )
  })
  for (list_fun in list_of_funs) {
    assign(x = list_fun[["name"]], value = list_fun[["fun"]], envir = .GlobalEnv)
  }
}

add_spinner(c("plotOutput", "tableOutput"))

ui <- pageWithSidebar(
  headerPanel("Interactive Histogram"),
  sidebarPanel(
    numericInput("lambda", "Lambda parameter", 
                 min = 0, value = 13),
    selectInput("colour", "Density plot colour",
                choices = c("red",
                            "red1",
                            "red2",
                            "red4"),
                selected = "red")
  ),
  mainPanel(
    plotOutputSpinner("density"),
    tableOutputSpinner("table")
  )
)


server <- function(input, output) {
  data <- reactive({
    rpois(n = 1000000, lambda = input[["lambda"]])
  })
  
  output[["density"]] <- renderPlot({
    d <- density(data())
    plot(d, main = paste0("Density of Poisson distribution with lambda parameter equal to ",
                          input[["lambda"]]))
    polygon(d, col = input[["colour"]])
  })
  
  output[["table"]] <- renderTable(
    table(data())
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
