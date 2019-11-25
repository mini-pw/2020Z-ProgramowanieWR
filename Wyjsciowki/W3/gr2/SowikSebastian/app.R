# adapted from https://gist.github.com/garrettgman/6673251

library(shiny)
library(shinycssloaders)
library(shinyhelper)

f_names <- c("numericInput", "sliderInput")

l <- lapply(f_names, function(ith_f_name){
  function(inputId, ...){
    helper(getFromNamespace(ith_f_name, ns="shiny")(inputId, ...), content=inputId, type="markdown")
  }
})

for (i in 1L:length(f_names)) {
  assign(paste0(f_names[i], "Helper"), l[[i]])
}



ui <- pageWithSidebar(
    headerPanel("Interactive Histogram"),
    sidebarPanel(
        selectInput("family", "From this family",
                    choices = c("Normal",
                                "Uniform",
                                "Exponential"),
                    selected = "normal"),
        downloadButton('downloadhist', 'DownloadHistogram'),
        downloadButton('downloaddens', 'DownloadDensity')
    ),
    mainPanel(
        withSpinner(helper(plotOutput("density"), content = "plotDensity", type = "markdown")),
        withSpinner(helper(plotOutput("histogram"), content = "plotHistogram", type = "markdown"))
    )
)


server <- function(input, output) {
    
    observe_helpers(withMathJax = TRUE, help_dir = "markdowny_pomocowe")
    
    data <- reactive({ 
        rpois(10e3,1)
    })
    
    output[["histogram"]] <- renderPlot({
        hist(data())
    })
    
    output[["density"]] <- renderPlot({
        plot(density(data()))
    })
    
    output$downloadhist <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.png', sep='')
      },
      content = function(con) {
        png(con)
        hist(data())
        dev.off()
      }
    )
    output$downloaddens <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.png', sep='')
      },
      content = function(con) {
        png(con)
        plot(density(data()))
        dev.off()
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
