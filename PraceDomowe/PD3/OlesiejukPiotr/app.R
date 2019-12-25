library(shiny)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(shinycssloaders)
library(shinyhelper)

# utiity functions

merge_lists <- function(l1, l2) {
    c(l1, list(l2))
}

v <- function(style, shortname) {
    paste0(style, shortname)
}

create_conditional_panel <- function(shortname) {
    conditionalPanel(
        condition = sprintf("input.is%s == true", shortname),
        htmlOutput(v("header", shortname)),
        uiOutput(v("UI", shortname))
    )
}

draw_brush <- function(input, shortname, ranges) {
    brush <- input[[sprintf("plot%s_brush", shortname)]]
    if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
        
    } else {
        ranges$x <- NULL
        ranges$y <- NULL
    }
}

# UI

ui <- fluidPage(
    titlePanel("Plot your data!"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Choose a CSV file"),
            checkboxInput(inputId = "isScat", label = "Scatterplot") 
                %>% helper(type = "markdown", content = "Menu"),
            checkboxInput(inputId = "isBar", label = "Barchart"),
            checkboxInput(inputId = "isHeat", label = "Heatmap"),
            # scatterplot
            create_conditional_panel('Scat'),
            # barchart
            create_conditional_panel('Bar'),
            # heatmap
            create_conditional_panel('Heat')
            
        ),
        mainPanel(
            conditionalPanel(
                condition = "input.isScat == true",
                plotOutput("plotScat",
                           dblclick = "plotScat_dblclick",
                           brush = brushOpts(
                               id = "plotScat_brush",
                               resetOnNew = TRUE
                           ))
                ) %>% helper(type = "markdown", content = "Plot"),
            conditionalPanel(
                condition = "input.isBar == true",
                plotOutput("plotBar",
                           dblclick = "plotBar_dblclick",
                           brush = brushOpts(
                               id = "plotBar_brush",
                               resetOnNew = TRUE
                           ))
            ),
            conditionalPanel(
                condition = "input.isHeat == true",
                plotOutput("plotHeat",
                           dblclick = "plotHeat_dblclick",
                           brush = brushOpts(
                               id = "plotHeat_brush",
                               resetOnNew = TRUE
                           ))
                )
            
        )
    )   
)

# SERVER

server <- function(input, output) {
    
    observe_helpers()
    
    rangesScat <- reactiveValues(x = NULL, y = NULL )
    rangesBar <- reactiveValues(x = NULL, y = NULL )
    rangesHeat <- reactiveValues(x = NULL, y = NULL )
    
    # Scatterplot UI
    output_ui(input, output, 'Scatterplot', 'Scat')
    
    # barplot UI
    output_ui(input, output, 'Barplot', 'Bar')
    
    # heatmap UI
    output_ui(input, output, 'Heatmap', 'Heat')
    
    observeEvent(input$buttonScat, {  
        render_plot_observed(input, output, 'Scat', 'Scatterplot',  'geom_point', rangesScat) })
    
    observeEvent(input$buttonBar, {
        render_plot_observed(input, output, 'Bar', 'Barplot',  'geom_bar', rangesBar) })
    
    observeEvent(input$buttonHeat, {
        render_plot_observed(input, output, 'Heat', 'Heatplot',  'geom_tile', rangesHeat) })
    
    observeEvent(input$plotScat_dblclick, {
        draw_brush(input, 'Scat', rangesScat)
    })
    
    observeEvent(input$plotBar_dblclick, {
        draw_brush(input, 'Bar', rangesBar)
    })
    
    observeEvent(input$plotHeat_dblclick, {
        draw_brush(input, 'Heat', rangesHeat)
    })
    
}


get_legend <- function(input, shortname) {
    colValue <- input[[v('col', shortname)]]
    sizeValue <- input[[v('size', shortname)]]
    fillValue <- input[[v('fill', shortname)]]
    if (colValue != 'None') color <- colValue
    else color <- ""
    if (sizeValue != 'None') size <- sizeValue
    else size <- ""
    if (fillValue != 'None') fill <- fillValue
    else fill <- ""
    list(color = color, size = size, fill = fill)
}

set_value <- function(value, standard = NULL) {
    if (value != 'None') {
        return(sym(value))
    }
    standard
}

get_aes <- function(input, shortname) {
    mappings <- c("x", "y", "col", "size", "fill")
    names <- paste0(mappings, shortname)
    my_aes <- list()
    for (name in names) {
        #browser()
        if (input[[name]] != 'None') {
            if (name %in% names[1:2]) {
                my_aes <- c(my_aes, sprintf("eval(sym(input$%s))", name))
            } else {
                my_aes <- c(my_aes, sprintf("eval(set_value(input$%s, NULL))", name))
            }
        }
        else my_aes <- c(my_aes, 'None')
    }
    names(my_aes) <- mappings
    my_aes <- my_aes[if_else(my_aes != 'None', TRUE, FALSE)]
    do.call(aes_string, my_aes)
}

create_plot <- function(input, datapath, shortname, title, plot_type, ranges) {
    dat <- read.csv(datapath, stringsAsFactors = TRUE)
    # custom legend
    legend <- get_legend(input, shortname)
    my_aes <- get_aes(input, shortname)
    #browser()
    p <- ggplot(data =  dat, 
           mapping = my_aes
    ) + 
        get(plot_type)() +
        theme_fivethirtyeight() +
        ggtitle(title) +
        xlab(input[[v("x", shortname)]]) +
        ylab(input[[v("y", shortname)]]) +
        theme(plot.title = element_text(hjust = 0.5), 
              axis.title = element_text(),
              panel.background = element_blank(),
              plot.background = element_blank(),
              legend.background = element_blank()) +
        coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
        guides(color = guide_legend(title = legend$color),
               size = guide_legend(title = legend$size),
               fill = guide_legend(title = legend$fill))
    if (input[[v('facet', shortname)]] != 'None'){
        p <- p + facet_wrap(sym(input[[v('facet', shortname)]]))
    }
    p
}

create_ui <- function(shortname = "Scat", attributes_logicals, data_file) {
    # logicals vector of true false for (x,y,col, fill, size)
    attrs <- c("x", "y", "col", "fill" ,"size", "facet")
    descriptions <- c("x-axis variable", "y-axis variable", "Color variable",
                      "Fill variable", "Size variable", "Facet variable")
    good_attrs <- attrs[attributes_logicals]
    good_descr <- descriptions[attributes_logicals]
    
    options <- colnames(read.csv(data_file))
    
    l <- lapply(seq(good_attrs), function(i, good_descr, good_attrs, shortname, options) { 
        selectInput(
            paste0(good_attrs[i], shortname, collapse = ''),
            good_descr[i],
            c('None', options)
            ) }, good_descr, good_attrs, shortname, options)
    l <- merge_lists(l, actionButton(paste0("button", shortname), "Generate plot"))
    l
}

output_ui <- function(input, output, title, shortname) {
    full_title <- sprintf("<br><h4><b>%s</b></h4><br>", title)
    output[[v('header', shortname)]] <- renderUI(HTML(full_title))
    
    output[[v('UI', shortname)]] <- renderUI({
        if (!is.null(input$file)) {
            f <- input$file
            create_ui(shortname, rep(TRUE, 5), f$datapath)
        }
    })
    output
}

render_plot_observed <- function(input, output, shortname, title, plot_type, ranges) {
    output[[v('plot', shortname)]] <- renderPlot({
        if (input[[v('is', shortname)]] == TRUE) {
            if (!is.null(input$file)) {
                # data processing
                f <- input$file
                create_plot(input, f$datapath, shortname, title, plot_type, ranges)
            }
        }
    })
    output
}

shinyApp(ui = ui, server = server)


