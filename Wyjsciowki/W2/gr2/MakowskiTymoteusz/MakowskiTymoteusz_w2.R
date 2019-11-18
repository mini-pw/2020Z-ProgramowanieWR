library(shiny)
library(ggplot2)

plot_dat <- data.frame(x = 1L:10, y = 10L:1)

ui <- fluidPage(
    plotOutput("points_plot", click = "plot_click"),
    tableOutput("points_table"))

server <- function(input, output, session) {

    marked_points <- reactiveValues("marked" = rep(FALSE, 10))


    observeEvent(input[["plot_click"]], {
        old_values <- isolate(marked_points[["marked"]])
        new_click <- nearPoints(plot_dat, input[["plot_click"]])

        marked_points[["marked"]][new_click$x] <- !old_values[new_click$x]
    })


    output[["points_table"]] <- renderTable(
        plot_dat[marked_points[["marked"]], ]
    )


    output[["points_plot"]] <- renderPlot({
        sizes <- c(1, 10)[marked_points[["marked"]] + 1]

        ggplot(plot_dat, aes(x = x, y = y)) +
            geom_point(size = sizes)
    })
}

shinyApp(ui = ui, server = server)
