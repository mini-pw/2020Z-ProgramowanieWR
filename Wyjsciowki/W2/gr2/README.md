Na podstawie poniższej aplikacji Shiny stwórz aplikację Shiny z interaktywnym obrazkiem, gdzie kliknięte punkty są powiększane, a ich współrzędne pojawiają się w tabeli pod obrazkiem.

```R
library(shiny)
library(ggplot2)

plot_dat <- data.frame(x = 1L:10, y = 10L:1)

ui <- fluidPage(plotOutput("points_plot"))

server <- function(input, output, session) {
    
    output[["points_plot"]] <- renderPlot({
        
        ggplot(plot_dat, aes(x = x, y = y)) +
            geom_point()
        
    })
    
}

shinyApp(ui = ui, server = server)
```

Rozwiązanie zadania dodajemy w folderze o nazwie NazwiskoImie.
