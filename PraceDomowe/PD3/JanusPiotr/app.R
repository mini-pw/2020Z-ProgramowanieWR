
library(shiny)
library(ggplot2)
library(shinyjs)

ui <- fluidPage(
    fluidRow(
        sidebarLayout(
            sidebarPanel(
                useShinyjs(),
            
               checkboxInput("use_header",label = "Use header", value = TRUE),
               fileInput("input_file", "Choose CSV file",
                         accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                         ),
               selectInput("plot_type", "Select plot type", choices = c("Scatterplot",
                                                                        "Heatmap",
                                                                        "Barchart",
                                                                        "Line plot",
                                                                        "2D Density")),
               fluidRow(
                   column(6, selectInput("xaxis", label = "Select first variable", choices = c(""))),
                   column(6, selectInput("yaxis", label = "Select second variable", choices = c("")))    
               ),
               selectInput("facet", label = "Select facet variable", choices = c("")),
               selectInput("color", label = "Select color variable", choices = c("")),
               actionButton("plot_button", label = "Plot")
               ),
            
            mainPanel(
                div(
                    style = "position:relative",
                    plotOutput("plot", 
                               hover = hoverOpts("plot_hover", 
                                                 delay = 100, 
                                                 delayType = "debounce"),
                               dblclick = "plot_dblclick",
                               brush = brushOpts(
                                   id = "plot_brush",
                                   resetOnNew = TRUE
                               )),
                    uiOutput("hover_info")
                )
            )
              
        )
    )
)

server <- function(input, output, session) {
    
    rv <- reactiveValues(curr_data = NA, data = NULL)
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    observeEvent(input$input_file, {
        rv$data <- read.csv(input$input_file$datapath, header = as.logical(input$use_header))
        rv$curr_data <- colnames(rv$data)
        col_names <- colnames(rv$data)
        id_list <- c("xaxis", "yaxis", "color", "facet")
        lapply(id_list, function(id) {
            if(!grepl("axis", id)) {
                col_names <- c("none", col_names)
            }
            updateSelectInput(session = session, inputId = id, choices = col_names)
            })
    })

    observeEvent(input$plot_button, {
        make_chart()
    })
    
    change_state <- function(item_id) {
      # browser()
      if(!is.null(input$input_file)) {
        if (input$plot_type == "Barchart") 
          shinyjs::disable("yaxis")
        if (item_id == "plot_button"  & input$xaxis != "" & input$yaxis != "")
          shinyjs::enable(item_id)
        if (item_id != "plot_button")
          shinyjs::enable(item_id)
      }
      if(is.null(input$input_file)){
        shinyjs::disable(item_id)
      }
    }
    observe({
        id_list <- c("xaxis", "yaxis", "color", "facet", "plot_button")
        lapply(id_list, function(id) change_state(id))
    })
    
    
    make_chart <- function() {
        args = list(
            data = rv$data,
            x = input$xaxis,
            y = input$yaxis,
            facet = input$facet,
            plot_type = input$plot_type
        )
        if (input$color != "none") {
            if (input$plot_type %in% c("Line plot", "Scatterplot"))
                args$color <- input$color
            else
                args$fill <- input$color
        }
        g <- do.call(plot_some_crazy_charts, args = args)
        output$plot <- renderPlot(g) 
    }
    
    plot_some_crazy_charts <- function(data, x, y, facet, plot_type, ...) {
        g <- ggplot(data, aes_string(x = x, y = y, ...)) +
            theme_minimal() +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = TRUE)
        # browser()
        type <- switch(plot_type, 
                       "Heatmap" = geom_tile,
                       "Scatterplot" = geom_point,
                       "Barchart" = geom_col,
                       "Line plot" = geom_line,
                       "2D Density" = geom_density2d)
        g <- g + type() 
        
        if (!is.null(facet) & facet != "none")
            g <- g + facet_wrap(as.formula(paste("~", facet)))
        
        g
    }
    

    output$hover_info <- renderUI({
        if (is.null(rv$data)) return(NULL)
        hover <- input$plot_hover
        point <- nearPoints(rv$data, hover, threshold = 5, maxpoints = 1)
        if (nrow(point) == 0) return(NULL)

        style <- paste0("position:absolute; z-index:100; background-color: rgba(68, 68, 68, 0.45); ",
                        "left:", hover$coords_css$x + 2, "px; top:", hover$coords_css$y + 2, "px;")

        wellPanel(
            style = style,
            p(HTML(paste0(sprintf("<b> %s </b>",input$xaxis), point[, input$xaxis], "<br/>",
                          sprintf("<b> %s </b>",input$yaxis), point[, input$yaxis], "<br/>")))
        )
    })
    observeEvent(input$plot_brush ,{
        brush <- input$plot_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
        make_chart()
    })
    
    observeEvent(input$plot_dblclick,{
        ranges$x <- NULL
        ranges$y <- NULL
        make_chart()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
