library(shiny)

shinyServer(function(input, output) {
  output$summary_output <- renderPrint({
    summary(read.csv(input$data_input$datapath))
  })
  
  output$data_output <- DT::renderDataTable({
    read.csv(input$data_input$datapath)
  }, editable = "cell")
  output$session_output <- renderPrint({
     sessionInfo()
  })
})