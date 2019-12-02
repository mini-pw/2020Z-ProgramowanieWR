library("data.table")
library("DT")
library("ggplot2")
library("shiny")
library("shinythemes")
library("tools")


# Utilities --------------------------------------------------------------------

# Trying to read the data with UTF-8 encoding and then with Latin-1 encoding if
# UTF-8 fails. If both fail then we handle an error.
read_data <- function(data_path, has_header, data_encoding = "UTF-8", errored = FALSE) {
    if (file_ext(data_path) != "csv") return("Error: provided file is not a csv.")
    
    data <- tryCatch(
        fread(data_path, header = has_header, encoding = data_encoding),
        warning = function(w) {
            if (errored) return("Error: could not load the uploaded file.")
            read_data(data_path, has_header, "Latin-1", TRUE)
        },
        error = function(e) {
            if (errored) return("Error: could not load the uploaded file.")
            read_data(data_path, has_header, "Latin-1", TRUE)
        }
    )
    
    if (!has_header) {
        colnames(data) <- paste("Column", seq_len(ncol(data)))
    }
    
    data
}


# Plotting interface for 3 different geometries utilising do.call and ellipsis.
plot_data <- function(geometry, df, ...) {
    geom_func <- switch(geometry, 
           "Heatmap" = geom_tile,
           "Barchart" = geom_bar,
           "Scatterplot" = geom_point
    )
    
    do.call(what = function(...) ggplot(df, aes_string(...)), args = list(...)) +
        geom_func() + theme_minimal()
}


# UI ---------------------------------------------------------------------------
ui <- fluidPage(
    theme = shinytheme("sandstone"),
    tags$head(
        tags$style(".selectize-dropdown {position: static}"),
        tags$style("#error_upload {color: red; font-size: 16px; font-style: bold;}"),
        tags$style("#error_plot {color: red; font-size: 16px; font-style: bold;}")
    ),
    
    titlePanel("PIY (Plot It Yourself)"),
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(id = "tabs",
                        
                # Data upload panel.
                tabPanel("Data upload", value = "panel_data_upload", br(),
                    checkboxInput("headers", label = "My data has headers", value = TRUE),
                    fileInput("input", label = "Upload a file",
                              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                    textOutput("error_upload")
                ),
                
                # Plot settings panel (with placeholder when the file is not uploaded).
                tabPanel("Plot settings", value = "panel_plot_settings", br(),
                    conditionalPanel("!output.data_uploaded", h5("Upload a file first.")),
                    conditionalPanel("output.data_uploaded",
                        selectInput("geometry", label = "Plot geometry",
                                    choices = c("Barchart", "Heatmap", "Scatterplot")),
                        uiOutput("plot_settings"),
                        actionButton("draw_plot", label = "Plot!"))
                )
            )
        ),
        mainPanel(
            conditionalPanel("!output.data_uploaded", h5("Upload a file to create a plot.")),
            conditionalPanel("output.data_uploaded",
                tabsetPanel(
                    tabPanel("Plot", br(),
                        conditionalPanel("!output.data_plotted",
                                         h5("Select plot settings and press \"Plot!\" button.")),
                        conditionalPanel("output.data_plotted",
                                         textOutput("error_plot"),
                                         plotOutput("plot"))
                    ),
                    tabPanel("Data preview", br(), DT::dataTableOutput("data_preview"))
                )
            )
        )
    )
)


# Server -----------------------------------------------------------------------
server <- function(input, output, session) {
    data <- reactiveValues("df" = data.frame(), "error_upload" = NULL,
                           "error_plot" = NULL,"plot" = FALSE)
    cNames <- reactive(colnames(data[["df"]]))
    
    # For conditional panel with Plot settings.
    output[["data_uploaded"]] <- reactive(nrow(data[["df"]]) > 0)
    outputOptions(output, "data_uploaded", suspendWhenHidden = FALSE)
    
    # For placeholder text in plot panel after data upload.
    output[["data_plotted"]] <- reactive(data[["plot"]])
    outputOptions(output, "data_plotted", suspendWhenHidden = FALSE)
    observeEvent(input[["draw_plot"]], data[["plot"]] <- TRUE)
    
    observeEvent(input[["input"]], {
        data_path <- input[["input"]][["datapath"]]
        input_data <- read_data(data_path, input[["headers"]])
    
        data[["error_upload"]] <- input_data
        if (is.data.table(input_data)) {
            data[["df"]] <- as.data.frame(input_data)
            data[["error_upload"]] <- NULL
            data[["plot"]] <- FALSE
            updateTabsetPanel(session, "tabs", selected = "panel_plot_settings")
        }
    })
    
    output[["error_upload"]] <- renderText(data[["error_upload"]])
    output[["error_plot"]] <- renderText(data[["error_plot"]])
    
    # Dynamic settings for different geometries.
    output[["plot_settings"]] <- renderUI(
        list(
            switch(input[["geometry"]],
                   "Heatmap" = c(
                       splitLayout(
                           selectInput("x", label = "X-axis variable", choices = cNames()),
                           selectInput("y", label = "Y-axis variable", choices = cNames())),
                       selectInput("fill", label = "Fill variable", choices = cNames())
                   ),
                   "Barchart" = selectInput("x", label = "X-axis variable", choices = cNames()),
                   "Scatterplot" = splitLayout(
                       selectInput("x", label = "X-axis variable", choices = cNames()),
                       selectInput("y", label = "Y-axis variable", choices = cNames()))
            ),
            splitLayout(
                selectInput("facet", label = "Facet grid variable", choices = c("<None>" = "...<None>...", cNames())),
                selectInput("facet_dim", label = "Facet grid dimension", choices = c("Rows" = "rows", "Cols" = "cols"))
            )
        )
    )
    
    # Event reactive which outputs plot (only when the button is pressed).
    plot_obj <- eventReactive(c(input[["draw_plot"]], data[["plot"]]), {
        if (!data[["plot"]]) return(NULL)
        
        call_args <- switch(input[["geometry"]],
                            "Heatmap" = list(
                                x = input[["x"]],
                                y = input[["y"]],
                                fill = input[["fill"]]),
                            "Barchart" = list(
                                x = input[["x"]]),
                            "Scatterplot" = list(
                                x = input[["x"]],
                                y = input[["y"]])
        )
        
        g <- tryCatch(
            do.call(what = function(...) plot_data(input[["geometry"]], data[["df"]], ...),
                    args = call_args),
            error = function(e) {
                data[["error_plot"]] <- "Error: could not plot provided data."
                return(NULL)
        })

        if (input[["facet"]] != "...<None>...") {
            facet_args <- list("rows" = NULL, "cols" = NULL)
            facet_args[[input[["facet_dim"]]]] <- vars(!!as.name(input[["facet"]]))
            
            g <- do.call(function(...) g + facet_grid(...), facet_args)
        }

        g
    })

    output[["plot"]] <- renderPlot(plot_obj())
    
    output[["data_preview"]] <- DT::renderDataTable(data[["df"]])
}


shinyApp(ui = ui, server = server, options = c("port" = 8080))


# TODO: Dodać inne wymiary:
#  - Kolor
#  - Kształt linii
# TODO: Dodać interaktywność:
#  - Przybliżanie i oddalanie
#  - Tooltipy z wartością
# TODO: README.md z dodanym linkiem do shinyapps.