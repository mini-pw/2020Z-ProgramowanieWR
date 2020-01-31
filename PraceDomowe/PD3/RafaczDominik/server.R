library(data.table)
library(ggplot2)
library(plotly)

server <- function(input, output) {
  output$data_info <- renderText({
    if (is.null(input$data)) {
      "Please, select input file"
    } else {
      "Your data:"
    }
  })
  
  dat <- reactive({
    if (is.null(input$data)) {
      list()
    } else {
      data.table::fread(input$data$datapath)
    }
  })
  
  output$data_table <- renderTable({
    dat()
  })
  
  aeses <- reactive({
    switch(input$plot_type,
           barchart = c("x", "y", "alpha", "colour", "fill", "group", "linetype", "size"),
           heatmap = c("x", "y", "fill", "group"),
           scatterplot = c("x", "y", "alpha", "colour", "fill", "group", "shape", "size", "stroke"))
  })
  
  output$variables_bindings <- renderUI({
    lapply(aeses(), function(aesthetic) {
      selectInput(inputId = paste0("bind_", aesthetic),
                  label = paste0("Binding for ", aesthetic),
                  choices = c("NO BINDING", colnames(dat())))
    })
  })
  
  output$faceting_bindings <- renderUI({ 
    selectInput(inputId = "facet_variable",
                  label = paste0("Variable to facet"),
                  choices = c("NO FACETING", colnames(dat())))
  })
  
  aes_binded <- reactive({
    in_names <- names(input)
    binded_names <- in_names[grepl("bind_[a-z]*$", in_names)]
    binds <- sapply(binded_names, function(name) {
      input[[name]]
    })
    binds <- binds[binds != "NO BINDING"]
    names(binds) <- gsub("bind_", "", names(binds))
    do.call(aes_string, as.list(binds))
  })
  
  created_plot <- reactive({
    p <- ggplot(data = dat(), mapping = aes_binded()) 
    p <- p + switch(input$plot_type,
                    barchart = geom_bar(),
                    heatmap = geom_bin2d(),
                    scatterplot = geom_point())
    if (input$facet_variable != "NO FACETING") {
      p <- p + facet_wrap(as.formula(paste0("~", input$facet_variable)))
    }
    p
  })
  
  output$rendered_plot <- renderPlot({
    created_plot()
  })
}