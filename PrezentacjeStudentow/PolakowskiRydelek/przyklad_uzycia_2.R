### Kod przykladu
library("future")

plan(sequential)
demo("mandelbrot", package = "future", ask = FALSE)

plan(multiprocess)
demo("mandelbrot", package = "future", ask = FALSE)
