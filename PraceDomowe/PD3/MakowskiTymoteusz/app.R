library("shiny")
library("ggplot2")
library("data.table")
library("tools")


# Trying to read the data with UTF-8 encoding and then with Latin-1 encoding if
# UTF-8 fails. If both fail then we handle an error.
read_data <- function(data_path, has_header, data_encoding = "UTF-8", errored = FALSE) {
    if (file_ext(data_path) != "csv") return("Error: provided file is not a csv.")
    
    tryCatch(
        fread(data_path, header = has_header, encoding = data_encoding),
        warning = function(w) {
            fread(data_path, header = has_header, encoding = data_encoding)
        },
        error = function(e) {
            if (errored) return("Could not load uploaded file.")
            read_data(data_path, has_header, "Latin-1", TRUE)
        }
    )
}


plot_data <- function(geometry, df, ...) {
    plot_func <- switch(geometry, 
           "Heatmap" = function(x, y, fill) {
               ggplot(df, aes_string(x = x, y = y, fill = fill)) +
                   geom_tile()
            }
    )
    
    do.call(plot_func, list(...))
}


ui <- pageWithSidebar(
    headerPanel("An app."), # TODO
    sidebarPanel(
        tabsetPanel(id = "tabs",
            tabPanel("Data upload", value = "panel_data_upload", br(),
                fileInput("input", label = "Upload a file",
                          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                checkboxInput("headers", label = "Data has headers", value = TRUE),
                textOutput("error")
            ),
            tabPanel("Plot settings", value = "panel_plot_settings", br(),
                conditionalPanel("!output.data_uploaded", h5("Please upload a file first.")),
                conditionalPanel("output.data_uploaded",
                    selectInput("geometry", label = "Plot geometry",
                                choices = c("Heatmap", "Barchart", "Scatterplot")),
                    uiOutput("plot_settings"),
                    actionButton("draw_plot", label = "Plot!"))
            )
        )
    ),
    mainPanel(
        tags$head(
            tags$style(".selectize-dropdown {position: static}"),
            tags$style("#error{color: red; font-size: 16px; font-style: bold;}")
        ),
        conditionalPanel("!output.data_uploaded", h5("Upload file to create a plot.")),
        plotOutput("plot")
    )
)


server <- function(input, output, session) {
    data <- reactiveValues("df" = data.frame(), "error" = NULL)
    cNames <- reactive(colnames(data[["df"]]))
    
    # For conditional panel with Plot settings.
    output[["data_uploaded"]] <- reactive(nrow(data[["df"]]) > 0)
    outputOptions(output, "data_uploaded", suspendWhenHidden = FALSE)
    
    observeEvent(input[["input"]], {
        data_path <- input[["input"]][["datapath"]]
        input_data <- read_data(data_path, input[["headers"]])
    
        data[["error"]] <- input_data
        if (is.data.table(input_data)) {
            data[["df"]] <- as.data.frame(input_data)
            data[["error"]] <- NULL
            updateTabsetPanel(session, "tabs", selected = "panel_plot_settings")
        }
    })
    
    output[["error"]] <- renderText(data[["error"]])
    
    output[["plot_settings"]] <- renderUI(
        switch(input[["geometry"]],
               "Heatmap" = list(
                   splitLayout(
                       selectInput("x", label = "X-axis variable",
                                   selected = NULL, choices = cNames()),
                       selectInput("y", label = "Y-axis variable",
                                   selected = NULL,  choices = cNames())),
                   selectInput("fill", label = "Fill variable",
                               selected = NULL, choices = cNames())
               ),
               "Barchart" = {
                   
               })
    )
    
    plot_obj <- eventReactive(input[["draw_plot"]], {
        call_args <- switch(input[["geometry"]],
                            "Heatmap" = list(x = input[["x"]],
                                             y = input[["y"]],
                                             fill = input[["fill"]])
        )
        
        do.call(what = function(...) plot_data(input[["geometry"]], data[["df"]], ...),
                args = call_args)
    })
        
    output[["plot"]] <- renderPlot(plot_obj())
}


shinyApp(ui = ui, server = server, options = c("port" = 8080))



# TODO
# 1. Wczytywanie pliku .csv
#  - Parametr, czy są nazwy kolumn
#  - Sprawdzenie poprawności danych (liczba kolumn poszczególnych wierszy)
#  - 
#     
# 2. Rysowanie wykresu
#  - Wybór typu wykresu
#  - Wybór zmiennych z pliku na wymiary wykresu
# 
# 3. Aktualizacja wykresu, żeby był interaktywny.