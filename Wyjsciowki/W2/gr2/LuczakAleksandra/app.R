library(shiny)
library(ggplot2)

plot_dat <- data.frame(x = 1L:10, y = 10L:1)

ui <- fluidPage(
    plotOutput("points_plot", height = 600, click = "point_click"),
    tableOutput("plot_value"))

server <- function(input, output, session) {
    selected_points<- reactiveValues(
        selected = rep(FALSE,10)
    )
    #
    observeEvent(input[["point_click"]], {
        selected_points$selected[ nearPoints(plot_dat, input[["point_click"]],
           maxpoints = 1)$x]<-TRUE})

    output[["points_plot"]] <- renderPlot({
        sizes <-rep(1,10)
        sizes[selected_points$selected] <- 5
        ggplot(plot_dat,aes(x = x, y = y)) +
            geom_point(size = sizes)

    })
    output$plot_value <-renderTable({
        plot_dat[selected_points$selected,]
    })


}

shinyApp(ui = ui, server = server)


