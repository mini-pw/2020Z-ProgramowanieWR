library(shiny)

shinyUI(pageWithSidebar(

  headerPanel("Summary of the data"),

  sidebarPanel(
      fileInput("data_input", "Data input", accept = ".csv")
  ),

  mainPanel(
    verbatimTextOutput("summary_output"),
    dataTableOutput("data_output"),
    verbatimTextOutput("session_output")
  )
))