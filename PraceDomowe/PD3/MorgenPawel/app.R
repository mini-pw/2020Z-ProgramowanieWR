library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinycssloaders)


dict2pol <- list(color = "kolor",
                 size = "rozmiar",
                 fill = "kolor",
                 alpha = "przezroczystość",
                 y = "y")
availableAttributes <- list(
  scatter = list(Y = "y", Kolor = "color", Rozmiar = "size", Przezroczystość = "alpha", "Linia trendu" = "smooth"),
  bar = list(Y = "y", Kolor = "fill"),
  heat = list(Y = "y", Kolor = "fill"),
  boxplot = list(Y = "y", Kolor = "color"),
  density = list(Y = "y", Kolor = "fill", Przezroczystość = "alpha")
)
attributesWidgets <- list(
  y = "column",
  color = "column",
  fill = "column",
  size = "column",
  alpha = "slider",
  smooth = "checkbox"
)

ui <- fluidPage(
  titlePanel("Praca domowa 3"),
  sidebarLayout(sidebarPanel(
  tabsetPanel(
  tabPanel(
    "Krok 1",
    fileInput(
      'datafile',
      'Wybierz plik CSV',
      accept = c('text/csv', 'text/comma-separated-values,text/plain'),
      buttonLabel = "Przeglądaj...",
      placeholder = "Nie wybrano żadnego pliku"
    ),
    selectInput("plotType", "Wybierz typ wykresu", choices = list("Wykres punktowy"="scatter",
                                                                  "Wykres słupkowy"="bar",
                                                                  "Mapa ciepła"="heat",
                                                                  "Wykres skrzynkowy" = "boxplot",
                                                                  "Wykres gęstości" = "density")),
    helpText("Uwaga: narzędzie nie służy do processingu danych. Zakładamy, że dane są w pełni przetworzone i gotowe do naniesienia na wykres.")
  ),
  tabPanel(
    "Krok 2",
    htmlOutput("attributes"),
    htmlOutput("columns"),
    textInput("facetInput", label = "Facetuj:", placeholder = "R-owa formula, np x~y"),
    actionButton("renderButton", "Generuj wykres")
  ),
  tabPanel("Krok 3",
    textInput("titleInput",
              "Wpisz tytuł wykresu"),
    textInput("subtitleInput",
              "Wpisz podtytuł wykresu"),
    textInput("captionInput",
              "Wpisz podpis wykresu"),
    textInput("xLabelInput",
              "Zmień podpis osi x",
              placeholder = "Domyślnie nazwa kolumny"),
    textInput("yLabelInput",
              "Zmień podpis osi y",
              placeholder = "Domyślnie nazwa kolumny"),
    selectInput("themeInput",
                "Wybierz motyw wykresu",
                choices = c("gray",
                            "bw",
                            "line_draw",
                            "light",
                            "dark",
                            "minimal",
                            "classic",
                            "test")),
    actionButton("renderButton2", "Generuj wykres")
  )
)),
mainPanel(
  withSpinner(plotlyOutput("plot")))))

server <- function(input, output){
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  output[["attributes"]] <- renderUI({
    # Każdy typ wykresu ma określoną listę dostępnych atributów w availaleAttributes
    checkboxGroupInput("selectAttributes",
                       label = "Wybierz atrybuty",
                       choices = availableAttributes[[input[["plotType"]]]],
                       selected = if_else(input[["plotType"]] %in% c("bar", "density"),
                                          "",
                                          "y"))
  })
  
  selectedValues <- reactiveValues()
  output[["columns"]] <- renderUI({
    selected <- input[["selectAttributes"]]
    # Chcemy pamiętać wybrane kolumny (by np przy wyborze dodatkowych atrybutów się nie resetowały)
    for(selection in c(selected, "x")){
      if(is.null(input[[paste0(selection, "Input")]])) next
      selectedValues[[selection]] <- input[[paste0(selection, "Input")]]
    }
    inputs <- list(selectInput("xInput",
                               "Wybierz kolumnę x",
                               choices = names(filedata()),
                               selected = selectedValues[["x"]])
    )
    
    # Konwencja:
    # użytkownik wybiera atrybuty z dostępnego checkboxa (id: selectAttributes)
    # w zależności od wyboru pojawiają się inputy proszące o przypisanie wartości do atrybutów
    # inputy mają id następujące: NazwaAtrubutuInput. Czyli np atrybut size wprowadzamy za pomocą sizeInput
    
    createUI <- function(attribute){
      if(attributesWidgets[[attribute]] == "column")
        return(selectInput(paste0(attribute, "Input"),
                           paste0("Wybierz kolumnę ", dict2pol[[attribute]]),
                           choices = names(filedata()),
                           selected = selectedValues[[attribute]]))
      if(attributesWidgets[[attribute]] == "slider")
        return(sliderInput(paste0(attribute, "Input"),
                           paste0("Wybierz parametr ", dict2pol[[attribute]]),
                           min = 0, 
                           max = 1, 
                           value = 1))
    }
    inputs <- append(inputs,
                     values = lapply(selected,
                                     createUI))
    do.call(tagList, inputs)
  })
  plotToRender <- reactiveVal(NULL)
  updatePlot <- function(){
    selected <- input[["selectAttributes"]]
    # Chcemy działać na tych wartościach, które mają wartość "column"
    columns <- selected[attributesWidgets[selected] == "column"]
    geomStr <- lapply(c("x",columns), function(attribute){
      paste0(attribute,
             " = ",
             input[[paste0(attribute,"Input")]])
    })
    geomExpr <- paste0("aes(",
                       do.call(paste, append(geomStr, list(sep = ", "))),
                       ")") %>% str2expression()
    
    out <- ggplot(data = filedata(),
                  mapping = eval(geomExpr)) 
    if(input[["plotType"]] == "scatter"){
      if("smooth" %in% input[["selectAttributes"]]) out <- out + geom_smooth()
      if("alpha" %in% selected) out <- out + geom_point(alpha = input[["alphaInput"]])
      else out <- out + geom_point()
    }
    if(input[["plotType"]] == "bar"){
      out <- tryCatch(out + geom_bar(),
                      error = function(e) out + geom_col())
    }
    if(input[["plotType"]] == "boxplot"){
      out <- out + geom_boxplot()
    }
    if(input[["plotType"]] == "heat"){
      out <- out + geom_tile()
    }
    if(input[["plotType"]] == "density"){
      out <- out + geom_density()
    }
    out <- tryCatch(out + facet_grid(as.formula(input[["facetInput"]])),
                    error = function(e) {
                      if(input[["facetInput"]] != "")
                      warning("Formula is not correct")
                      out
                      })
    out <- out +
      xlab(if_else(input[["xLabelInput"]] != "",
                   input[["xLabelInput"]],
                   input[["xInput"]])) +
      ylab(if_else(input[["yLabelInput"]] != "",
                   input[["yLabelInput"]],
                   if_else(!"y" %in% input[["selectAttributes"]],
                           "",
                           input[["yInput"]]))) +
      labs(title = input[["titleInput"]],
           subtitle = input[["subtitleInput"]],
           caption = input[["captionInput"]]) +
      eval(str2expression(paste0("theme_",input[["themeInput"]],"()")))
    plotToRender(ggplotly(out))
  }
  observeEvent(input[["renderButton"]],updatePlot())
  observeEvent(input[["renderButton2"]], updatePlot())
  output[["plot"]] <- renderPlotly(plotToRender())
  
}

shinyApp(ui = ui, server = server)
