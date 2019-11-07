library(shiny)
library(ggplot2)
library(dplyr)

plot_dat <- data.frame(x = 1L:10, y = 10L:1, ind=1:10)

ui <- fluidPage(
    plotOutput("points_plot", click = "plot_click"),
    tableOutput("table_output"))

server <- function(input, output, session) {

    selected_points <- reactiveValues(
        indices = c()
    )

    observeEvent(input[["plot_click"]], {
        nearest_point <- nearPoints(plot_dat, input[["plot_click"]], maxpoints = 1)[["ind"]]

        print(nearest_point)

        if(length(nearest_point) == 1) {
            if(nearest_point %in% selected_points[["indices"]]) {
                selected_points[["indices"]] <- selected_points[["indices"]][!selected_points[["indices"]] %in% nearest_point]
            }
            else {
                selected_points[["indices"]] <- unique(c(selected_points[["indices"]], nearest_point))
            }
        }
    })

    output[["points_plot"]] <- renderPlot({
        mutate(plot_dat, selected = ind %in% selected_points[["indices"]]) %>%
            ggplot(aes(x = x, y = y, size = selected)) +
            geom_point() +
            scale_size_manual(values = c(2, 10)) +
            theme_bw()
    })

    output[["table_output"]] <- renderTable({
        plot_dat[selected_points[["indices"]], c("x", "y")]
    })



}

shinyApp(ui = ui, server = server)
