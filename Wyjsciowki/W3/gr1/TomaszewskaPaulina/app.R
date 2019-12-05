
# adapted from https://gist.github.com/garrettgman/6673251

library(shiny)
library(shinycssloaders)
library(shinyhelper)
library(dplyr)

func_vec <- c("numericInput")
func_list <- lapply(func_vec, function(ith_fun) 
  function(inputId, ...) {
    helper(getFromNamespace(ith_fun, ns = "shiny")(inputId = inputId, ...),  
           content = inputId, type = "markdown")
  })

for(ith_fun_id in 1L:length(func_list)) {
  assign(x = paste0(func_vec[ith_fun_id], "Helper"), value = func_list[[ith_fun_id]])
}

ui <- pageWithSidebar(
  headerPanel("Interactive Histogram"),
  sidebarPanel(
    numericInputHelper("n", "Generate this many points", 
                       min = 1, value = 10e2),

    numericInputHelper("lambda", "Lambda param", 
                       min = 1, value = 2),
  ),
  mainPanel(
    helper(withSpinner(plotOutput("density")), content = "example-file", type = "markdown"),
    tableOutput("tab")
  )
)


server <- function(input, output) {
  observe_helpers(help_dir = "help-mds")
  
  data <- reactive({ 
    rpois(input[["n"]], input[['lambda']])
  })
  
  output[["density"]] <- renderPlot({
    plot(density(data()))
  })
  output[['tab']]<-renderTable(data() )
  
}

# Run the application 
shinyApp(ui = ui, server = server)