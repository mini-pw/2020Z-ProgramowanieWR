library(shiny)
library(shinycssloaders)
library(shinyhelper)

func_vec <- c("plotOutput", "tableOutput")
func_list <- lapply(func_vec, function(ith_fun) 
  function(outputId, ...) {
    withSpinner(getFromNamespace(ith_fun, ns = "shiny")(outputId = outputId, ...))
  })

for(ith_fun_id in 1:length(func_list)) {
  assign(x = paste0(func_vec[ith_fun_id], "Spinner"), value = func_list[[ith_fun_id]])
}

ui <- pageWithSidebar(
  headerPanel("Interactive histogram"),
  sidebarPanel(
    sliderInput("n", "Wybierz Lambde", 
                 min = 1, max = 10000, value = 5, step = 1)
  ),
  mainPanel(
    plotOutputSpinner("hist"),
    tableOutputSpinner("tabela")
    
  )
)


server <- function(input, output) {
  
  observe_helpers(help_dir = 'help_mds')
  
  data <- reactive({ 
    rpois(22222, input[["n"]])
  })
  
  output[["hist"]] <- renderPlot({
    (hist(data()))
  })
  
  output[["tabela"]] <- renderTable({
    table(data())
  })
  
}

shinyApp(ui = ui, server = server)