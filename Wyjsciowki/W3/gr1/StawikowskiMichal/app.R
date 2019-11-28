# adapted from https://gist.github.com/garrettgman/6673251

library(shiny)
library(shinycssloaders)
library(shinyhelper)

outputList <- c("plotOutput", "tableOutput")
list <- lapply(outputList, function(f_name)
    function(outputId, ...){
        withSpinner(getFromNamespace(f_name, ns="shiny")(outputId=outputId,...))
    })

for(i in 1:length(list)){
    assign(paste0(outputList[i],"Spinner"), value=list[[i]])
}


ui <- pageWithSidebar(
    headerPanel("Density and table"),
    sidebarPanel(
        numericInput("n", "Set lambda", 
                     min = 1, value = 10)
    ),
    mainPanel(
        plotOutputSpinner("histogram"),
        tableOutputSpinner("table")
    )
)


server <- function(input, output) {
    observe_helpers(help_dir = "help_mds")
    
    data <- reactive({ 
        rpois(100000, input[["n"]])
        
    })
    
    output[["histogram"]] <- renderPlot({
        d <- density(data())
        plot(d) 
        
    })
    
    output[["table"]] <- renderTable({
        table(data())
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)