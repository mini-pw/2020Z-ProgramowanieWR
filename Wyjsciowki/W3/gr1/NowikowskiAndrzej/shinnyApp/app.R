#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)
library(ggplot2)

inputs <- c("plotOutput", "tableOutput")
x <- lapply(inputs, function(f_name) {
  function(outputId, ...) {
    withSpinner(getFromNamespace(f_name, ns="shiny")(outputId=outputId, ...))
  }
})

for (i in seq_along(inputs)) {
  assign(paste0(inputs[[i]], 'Spinner'), x[[i]])
}



# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Poisson distribution"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         sliderInput("lambda",
                     "Value of lambda:",
                     min = 1,
                     max = 10,
                     value = 5,
                     step = 0.5)
      ),

      # Show a plot of the generated distribution
      mainPanel(
         plotOutputSpinner("distPlot"),
         tableOutputSpinner("distTable")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   data <- reactive({
     df <- data.frame(value=rpois(1000000, input$lambda))
   })

   output$distPlot <- renderPlot({
     ggplot(data(), aes(x=value)) +
       geom_density()
   })

   output$distTable <- renderTable({
     table(data())
   })
}

# Run the application
shinyApp(ui = ui, server = server)

