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
        selected_points[["indices"]] <- c(selected_points[["indices"]],
                                          nearPoints(plot_dat, input[["plot_click"]], maxpoints = 1)[["ind"]])

        if(length(selected_points[["indices"]])) {
            selected_points[["indices"]] <- unique(selected_points[["indices"]])
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
