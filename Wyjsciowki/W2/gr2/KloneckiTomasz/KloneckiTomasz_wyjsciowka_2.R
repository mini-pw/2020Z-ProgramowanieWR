library(shiny)
library(ggplot2)

plot_dat <- data.frame(x = 1L:10, y = 10L:1)

ui <- fluidPage(
    fluidRow(
        column(width = 10,
               h4('Plot:'),
               plotOutput("plot1", height = 300,
                          click = "plot1_click"
               )
        )
    ),
    fluidRow(
        column(width = 6,
               h4("Clicked coordinates"),
               verbatimTextOutput("click_info")
        )        
        )
    )

server <- function(input, output, session) {
    
    output$plot1 <- renderPlot({
        ggplot(plot_dat, aes(x = x, y = y)) +
            geom_point()
    })
        
    output$click_info <- renderPrint({
        nearPoints(plot_dat, input$plot1_click, addDist = FALSE)
    })
    
}

shinyApp(ui = ui, server = server)