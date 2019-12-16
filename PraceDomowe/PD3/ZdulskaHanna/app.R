library(shiny)
library(plotly)
library(ggplot2)

ui <- fluidPage(

    titlePanel("Gwiazdkowe ploty"),
    

    sidebarLayout(
        
        sidebarPanel(
            fileInput(
                "dane",
                "Select file .csv",
                accept = c(".csv")
            ),
            radioButtons("separator","Separator: ",choices = c(",", ";", ":"), selected=",",inline=TRUE),
            selectInput(
                "type",
                "Select type of plot",
                choices = c("Heatmap",
                            "Scatter plot",
                            "Bar plot",
                            "Density plot 2D",
                            "Density plot",
                            "Violin plot",
                            "Line plot")
            ),
            
            conditionalPanel( condition = "!input.type.localeCompare(\"Scatter plot\")",
                              sliderInput(
                                  "shape",
                                  "Select scatter shape",
                                  min = 0,
                                  max = 25,
                                  value = 16
                              )),
            conditionalPanel( condition = "!input.type.localeCompare(\"Bar plot\")",
                              checkboxInput("dodge", "Position dodge")),
            
            conditionalPanel( condition = "!input.type.localeCompare(\"Density plot\") || !input.type.localeCompare(\"Density plot 2D\") || !input.type.localeCompare(\"Line plot\")",
                              selectInput(
                                  "linetype",
                                  "Select linetype",
                                  choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "1F", "F1", "4C88C488", "12345678")
                              )),
            
            selectInput(
                "x",
                "Select X axis",
                choices = c()
            ),
            selectInput(
                "y",
                "Select Y axis",
                choices = c()
            ),
            selectInput(
                "color",
                "Select color",
                choices = c()
            ),
            selectInput(
                "facet",
                "Select facet",
                choices = c()
            )
        ),

        mainPanel(
            plotlyOutput("plot")
        )
    )
)


server <- function(input, output, session) {
    
    observe({
        upd <- input$dane
        
        if(!is.null(input$dane) & !is.null(input$separator)){
            
            dane <- read.csv(input$dane$datapath, sep = input$separator)
            
            updateSelectInput(session = session,
                              inputId = "x",
                              choices = colnames(dane))
            
            updateSelectInput(session = session,
                              inputId = "y",
                              choices = colnames(dane))
            
            updateSelectInput(session = session,
                              inputId = "color",
                              choices = colnames(dane))
            
            updateSelectInput(session = session,
                              inputId = "facet",
                              choices = c("None",colnames(dane)))
        }
    })
    
    output[["plot"]] <- renderPlotly({
        
        validate({
            need(input$dane, "Select filename")

        })
        
        dane <- read.csv(input$dane$datapath, sep = input$separator)
        p <- ggplot(dane, aes(x = dane[[input$x]], 
                              y = dane[[input$y]],
                              fill = dane[[input$color]],
                              color = dane[[input$color]])) +
            xlab(input$x) + 
            ylab(input$y) +
            facet_wrap(dane[[input$facet]])
        
        if(input$type == "Heatmap"){
            p2 <- p + geom_tile()
        }
        else if(input$type == "Scatter plot"){
            p2 <- p + geom_point(shape = input$shape)
        }
        else if(input$type == "Bar plot"){
            if(input$dodge){
                p2 <- p + geom_bar(stat = "identity", position = 'dodge') + ggtitle(input)
            } else {
                p2 <- p + geom_bar(stat = "identity")
            }
        }
        else if(input$type == "Density plot 2D"){
            p2 <- p + geom_density2d()
        }
        else if(input$type == "Density plot"){
            p2 <-ggplot(dane, aes(x = dane[[input$x]], fill = dane[[input$color]], color = dane[[input$color]])) +
                xlab(input$x) +
                #ylab(input$y) +
                facet_wrap(dane[[input$facet]]) + 
                geom_density(linetype = input$linetype)
        }
        else if(input$type == "Violin plot"){
            p2 <-p + 
                geom_violin()
        }
        else if(input$type == "Line plot"){
            p2 <- p + geom_line(linetype = input$linetype)
        }
        
        ggplotly(p2)
        

    })
    
    outputOptions(output, "plot", suspendWhenHidden = FALSE)  
}


shinyApp(ui = ui, server = server)
