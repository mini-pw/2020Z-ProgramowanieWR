library(ggplot2)
library(magrittr)
library(shiny)
library(shinycssloaders)
library(shinyjs)

factor_mappings <- c("color", "shape", "facet") 
continuous_mappings <- c("x", "y", "fill")
mappings <- c(continuous_mappings, factor_mappings)

get_mapping_id <- function(mapping_name) {
    paste0(mapping_name, "_mapping")
}

create_mapping_select_inputs <- function(mapping_names) {
    create_single_input <- function(mapping_name) {
        selectInput(
            inputId = get_mapping_id(mapping_name), 
            label = mapping_name,
            choices = NULL
        )
    }
    
    lapply(mapping_names, create_single_input)
}

create_tooltip_content <- function(hovered_point, mappings) {
    mapping_string <- sapply(mappings, function(mapping) {
        hover_point_mapping_value <- hovered_point[[mapping]]
        if (!is.null(hover_point_mapping_value)) {
            paste0("<p>", mapping, "=", hover_point_mapping_value, "</p>")
        } else {
            NA
        }
    })
    
    mapping_string[!is.na(mapping_string)] %>% 
        unique() %>% 
        paste(collapse = " ")
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyjs(),
    # Application title
    titlePanel("Shiny Plotter"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            # csv file input
            fileInput(
                inputId ="csv_data", 
                label = "Upload csv file", 
                accept = c("text/csv", ".csv")
            ),
            # plot type selection
            selectInput(
                inputId = "plot_type",
                label = "Select plot type",
                choices = c("scatter", "heatmap", "bar")
            ),
            # mappings
            create_mapping_select_inputs(mappings) 
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput(
                outputId = "csv_data_plot",
                hover = hoverOpts(
                    id = "data_plot_hover",
                    delay = 100
                ),
                brush = brushOpts("data_plot_brush", resetOnNew = TRUE),
                dblclick = "data_plot_dblclick"
            ) %>% withSpinner(),
            # dummy tooltip div
            div(id = "plot_tooltip")
        ),
    ),
    tags$script(src = "https://unpkg.com/popper.js@1"),
    tags$script(src = "https://unpkg.com/tippy.js@5"),
    includeScript("www/scripts.js"),
    includeCSS("www/style.css")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    csv_data <- reactive({
        req(input$csv_data)
        read.csv(input$csv_data$datapath)
    })
    
    plot_ranges <- reactiveValues(
        x = NULL,
        y = NULL
    )
    
    observeEvent(input$data_plot_hover, {
        req(csv_data())
        np <- nearPoints(csv_data(), input$data_plot_hover, maxpoints = 1)
        
        session$sendCustomMessage("hideTooltip", "")
        if (nrow(np) == 1) {
            hovered_point <- np[1, ]
            mappings <- input$data_plot_hover$mapping
            tooltipData <- list(
                x = input$data_plot_hover$coords_img$x,
                y = input$data_plot_hover$coords_img$y,
                content = create_tooltip_content(
                    hovered_point = hovered_point,
                    mappings = mappings
                )
            )
            session$sendCustomMessage("hoverTooltipHandler", tooltipData)
        }
    })
    
    observe({
        req(csv_data())
        
        sapply(continuous_mappings, function(mapping_name) {
            updateSelectInput(
                session = session,
                inputId = get_mapping_id(mapping_name),
                label = mapping_name,
                choices = c(names(csv_data()))
            )
        })
        
        sapply(factor_mappings, function(mapping_name) {
            updateSelectInput(
                session = session,
                inputId = get_mapping_id(mapping_name),
                label = mapping_name,
                choices = c("NULL", names(csv_data())[sapply(csv_data(), is.factor)])
            )
        })
    })
    
    observe({
        brush <- input$data_plot_brush
        if (!is.null(brush)) {
            plot_ranges$x <- c(brush$xmin, brush$xmax)
            plot_ranges$y <- c(brush$ymin, brush$ymax)
        }
    })
    
    observeEvent(input$data_plot_dblclick, {
        plot_ranges$x <- NULL
        plot_ranges$y <- NULL
    })
    
    output$csv_data_plot <- renderPlot({
        req(csv_data())
        for (map in mappings)(
            req(input[[get_mapping_id(map)]])
        )
        ggplot(
            csv_data(),
            aes_string(
                x = input$x_mapping,
                y = input$y_mapping,
                color = input$color_mapping,
                fill = input$fill_mapping,
                shape = input$shape_mapping
            )
        ) + switch(
            input$plot_type,
            scatter = geom_point(),
            heatmap = geom_tile(),
            bar = geom_col()
        ) + facet_wrap(formula(paste0(".~ ", input$facet_mapping))) +
            coord_cartesian(xlim = plot_ranges$x, ylim = plot_ranges$y)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
