library(shiny)
library(shinycssloaders)


func_vec <- c("plotOutput", "tableOutput")

func_list <- lapply(func_vec, function(f_name) 
  function(outputId, ...) {
    withSpinner(getFromNamespace(f_name, ns = "shiny")(outputId = outputId, ...))
  })

for(i in 1:length(func_list)) {
  assign(x = paste0(func_vec[i], "Spinner"), value = func_list[[i]])
}


ui <- pageWithSidebar(
  headerPanel("Wykresy gestosci"),
  sidebarPanel(
    sliderInput("lambda", "Lambda", min = 0.001, max = 100, value = 1)
  ),
  mainPanel(
    plotOutputSpinner("density"),
    tableOutputSpinner("counts_table")
  )
)


server <- function(input, output) {
  observe_helpers(help_dir = "help_mds")
  
  data <- reactive({ 
    rpois(100000, input[["lambda"]])
  })
  
  output[["density"]] <- renderPlot({
    plot(density(data()))
  })
  
  output[["counts_table"]] <- renderTable({
    table(data())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
