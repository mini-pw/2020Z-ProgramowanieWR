# adapted from https://gist.github.com/garrettgman/6673251

library(shiny)
library(shinycssloaders)
library(shinyhelper)

input <- c("sliderInput", "numericInput", "selectInput")
x <- lapply(input, function(f_name){
  function(inputId, ...){
    helper(getFromNamespace(f_name, ns = "shiny")(inputId = inputId, ...), content = inputId, type = "markdown" )
  }})
for(i in seq_along(input)){
  assign(paste0(input[[i]], "helper"), x[[i]])
}



input <- c("plotOutput", "tableOutput")
x <- lapply(input, function(f_name){
  function(inputId, ...){
    withSpinner(getFromNamespace(f_name, ns = "shiny")(inputId, ...))
  }})
for(i in seq_along(input)){
  assign(paste0(input[[i]], "Spinner"), x[[i]])
}



ui <- pageWithSidebar(
  headerPanel("Interactive Density"),
  sidebarPanel(
    numericInputhelper("n", "Generate this many points", 
                       min = 1, value = 10e2),
    numericInputhelper("lamda", "Wybierz lambde dla rozkldu P", 
                       min = 0, value = 1)
  ),
  mainPanel(
    helper(plotOutputSpinner("density"), content = "example-file", type = "markdown"),
    helper(tableOutputSpinner("tabela"))
  )
)


server <- function(input, output) {
  observe_helpers(help_dir = "help_mds")
  
  data <- reactive({ 
    rpois(input[["n"]], input[["lamda"]])
  })
  
  output[["density"]] <- renderPlot({
    plot(density(data()))
  })
  output[["tabela"]] <- renderTable(table(data()))
}

# Run the application 
shinyApp(ui = ui, server = server)
