library(DT)

ui <- fluidPage(

  titlePanel("Quick Shiny reminder"),
  sidebarPanel(
    fileInput("file","import file"),
    verbatimTextOutput("session_info")
  ),
  mainPanel(
    DT::dataTableOutput("tb"),
    verbatimTextOutput("tb_summary"),
  )
)

server <- function(input,output){
  values <- reactiveValues()

  observe({
      if (is.null(input$file)) {
        values$data <- NULL
      } else {
        values$data <- read.csv(input$file$datapath)
      }
  })

  output$tb <- DT::renderDataTable({
    DT::datatable(values$data, editable= TRUE)
  })

  output$tb_summary <- renderPrint({
    if(!is.null(values$data)) {
      summary(values$data)
    }
  })

  output$session_info <- renderPrint({
    sessionInfo()
  })
}
shinyApp(ui=ui, server=server)
