
library(shiny)
library(shinycssloaders)


outputList <- c("plotOutput", "tableOutput")
list <- lapply(outputList, function(f_name)
  function(outputId, ...){
    withSpinner(getFromNamespace(f_name, ns="shiny")(outputId=outputId,...))
  })

for(i in 1:length(list)){
  assign(paste0(outputList[i],"Spinner"), value=list[[i]])
}




ui <- pageWithSidebar(
    headerPanel("Density plot"),
    sidebarPanel(
        numericInput("lambda", "Choose lambda", 
                     min = 1, value = 1)
    ),
    mainPanel(
        plotOutputSpinner("density"),
        tableOutputSpinner("tab")
    )
)


server <- function(input, output) {
    observe_helpers(help_dir = "help_mds")
    
    data <- reactive({ 
        
        data <- rpois(1000000, lambda=input[["lambda"]])
    })
    
    output[["density"]] <- renderPlot({
        plot(density(data()))
    })
    
    output[["tab"]] <- renderTable({
      table(data())
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
