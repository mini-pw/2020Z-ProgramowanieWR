library(shiny)
library(ggplot2)

compute_distance <- function(cursor, data_column) {
  ((cursor - data_column)/max(data_column))^2
}

ui <- fluidPage(
  titlePanel("Flexible Hedgehog Plotter"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file",
                "Data source",
                accept = ".csv"),
      selectInput("type",
                  "Plot type",
                  list(
                    `heatmap` = "geom_tile",
                    `barchart` = "geom_bar",
                    `scatterplot` = "geom_point"
                  )),
      selectInput("xAxis",
                  "X axis",
                  list()),
      selectInput("yAxis",
                  "Y axis",
                  list()),
      selectInput("facet",
                  "Facet grid",
                  list(),
                  multiple = TRUE),
      div(id = "aesthetics"),
      actionButton("insert",
                   "Insert aesthetic")
    ),
    mainPanel(
      plotOutput("plot",
                 dblclick = "plotClick",
                 brush = brushOpts(
                   id = "plotBrush",
                   resetOnNew = TRUE
                 ),
                 hover = hoverOpts(
                   id = "plotHover"
                 )),
      tableOutput("tooltip")
    )
  )
)

server <- function(input, output, session) {
  ui_id_set <- numeric(0)
  ranges <- reactiveValues(
    x = NULL,
    y = NULL
  )
  
  data <- reactive({
    validate(
      need(input[["file"]], message = "No file set.")
    )
    read.csv(input[["file"]][["datapath"]])
  })
  
  observeEvent(input[["plotClick"]], {
    if (is.null(input[["plotBrush"]])) {
      ranges[["x"]] <- NULL
      ranges[["y"]] <- NULL
    } else {
      ranges[["x"]] <- c(input[["plotBrush"]][["xmin"]], input[["plotBrush"]][["xmax"]])
      ranges[["y"]] <- c(input[["plotBrush"]][["ymin"]], input[["plotBrush"]][["ymax"]])
    }
  })
  
  output[["plot"]] <- renderPlot({
    validate(
      need(input[["file"]], "Choose a file first! How can I plot no data?"),
      need(input[["xAxis"]], "Hey, would you mind setting X axis? Thanks!"),
      need(input[["type"]] == "geom_bar" || input[["yAxis"]] != "", "Oh, one more thing. Set Y axis maybe?")
    )
    aesthetic_list <- lapply(ui_id_set, function(id) input[[paste0("column", id)]])
    names(aesthetic_list) <- sapply(ui_id_set, function(id) input[[paste0("aesthetic", id)]])
    print(aesthetic_list)
    aesthetic_list[["x"]] <- input[["xAxis"]]
    if (input[["type"]] != "geom_bar") {
      aesthetic_list[["y"]] <- input[["yAxis"]]
    }
    p <- ggplot(data(), do.call("aes_string", aesthetic_list)) +
      eval(parse(text = input[["type"]]))() +
      coord_cartesian(xlim = ranges[["x"]], ylim = ranges[["y"]], expand = FALSE)
    if (isTruthy(input[["facet"]])) {
      p <- p + facet_wrap(input[["facet"]])
    }
    p
  })
  
  observe({
    updateSelectInput(
      session = session,
      inputId = "xAxis",
      choices = colnames(data()),
      selected = input[["xAxis"]]
    )
    updateSelectInput(
      session = session,
      inputId = "yAxis",
      choices = colnames(data()),
      selected = input[["yAxis"]]
    )
    updateSelectInput(
      session = session,
      inputId = "facet",
      choices = colnames(data()),
      selected = input[["facet"]]
    )
  })
  
  observeEvent(input[["insert"]], {
    if (length(ui_id_set) == 0) {
      ui_id <- 1
    } else {
      ui_id <- max(ui_id_set) + 1
    }
    insertUI(
      selector = "#aesthetics",
      ui = div(id = paste0("row", ui_id),
               textInput(paste0("aesthetic", ui_id),
                         "Aesthetic type"),
               selectInput(paste0("column", ui_id),
                           "Column",
                           colnames(data())),
               actionButton(paste0("remove", ui_id),
                            "Remove this aesthetic"),
               # why this br() doesn't do anything is beyond me
               br()),
      session = session
    )
    observeEvent(input[[paste0("remove", ui_id)]], {
      removeUI(
        selector = paste0("#row", ui_id),
        session = session
      )
      ui_id_set <<- setdiff(ui_id_set, ui_id)
    })
    ui_id_set <<- c(ui_id_set, ui_id)
  })
  
  output[["tooltip"]] <- renderTable({
    validate(
      need(input[["plotHover"]], "Hover over spot on plot whose hint to print."),
      need(input[["xAxis"]], message = "X axis is necessary."),
      need(input[["yAxis"]], message = "Y axis is necessary."),
      need(input[["type"]] == "geom_point", "Better be it scatterplot, or else...!")
    )
    distance <- sqrt(compute_distance(input[["plotHover"]][["x"]], data()[[input[["xAxis"]]]]) +
      compute_distance(input[["plotHover"]][["y"]], data()[[input[["yAxis"]]]]))
    data()[which.min(distance), ]
  })
}

shinyApp(ui = ui, server = server)

