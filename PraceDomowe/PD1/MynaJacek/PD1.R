library(lattice)
library(tidyverse)
library(graphics)
library(microbenchmark)
library(ggplot2)

set.seed(1)
vot <- rnorm(100, 15, 5)
vot <- sort(vot, decreasing = FALSE)
phon <- "t"

df1 <- data.frame(phon, vot)

vot <- rnorm(100, -60, 15)
vot <- sort(vot, decreasing = FALSE)
phon <- "d"
df2 <- data.frame(phon, vot)

my_df <- rbind(df1, df2)

mbm_a <- microbenchmark("lattice" = { 
                        for(i in 1:500){
                          a1 <- bwplot(vot ~ phon, data = my_df)
                        }
                      },
                      "ggplot2" = {
                        for(i in 1:500){
                          a2 <- ggplot(my_df, aes(x = phon, y = vot)) + 
                            geom_boxplot()
                        }
                      },
                      "graphics" = {
                        for(i in 1:500){
                          a3 <- boxplot(vot ~ phon, data = my_df)
                        }
                      })

a1

a2

a3

autoplot(mbm_a)

Temperature <- airquality$Temp

mbm_b <- microbenchmark("lattice" = { 
  for(i in 1:500){
    b1 <- histogram(~ Temperature, type = "count", nint = 30)
  }
},
"ggplot2" = {
  for(i in 1:500){
    b2 <- ggplot(airquality, aes(Temperature)) + 
      geom_histogram()
  }
},
"graphics" = {
  for(i in 1:500){
    b3 <- hist(Temperature, breaks = 30)
  }
})

b1

b2

b3

autoplot(mbm_b)

mbm_c <- microbenchmark("lattice" = { 
  for(i in 1:500){
    c1 <- barchart(GNP ~ Year, data = longley, stack = FALSE, horizontal = FALSE, col = c("grey"))
  }
},
"ggplot2" = {
  for(i in 1:500){
    c2 <- ggplot(data = longley, aes(x=Year, y=GNP)) + 
      geom_col()
  }
},
"graphics" = {
  for(i in 1:500){
    c3 <- barplot(GNP ~ Year, data = longley)
  }
})

c1

c2

c3

autoplot(mbm_c)

attach(mtcars)

mbm_d <- microbenchmark("lattice" = { 
  for(i in 1:500){
    d1 <- xyplot(mpg~wt,
                 main="Scatterplots by Cylinders and Gears",
                 ylab="Miles per Gallon", xlab="Car Weight")
  }
},
"ggplot2" = {
  for(i in 1:500){
    d2 <- ggplot(mtcars, aes(x=wt, y=mpg)) +
      geom_point() + 
      ggtitle("Scatterplots by Cylinders and Gears") + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      ylab("Miles per Gallon") + xlab("Car Weight")
  }
},
"graphics" = {
  for(i in 1:500){
    d3 <- plot.default(mpg~wt,
                       main="Scatterplots by Cylinders and Gears",
                       ylab="Miles per Gallon", xlab="Car Weight")
  }
})

d1

d2

d3

autoplot(mbm_d)

mbm_e <- microbenchmark("lattice" = { 
  for(i in 1:500){
    e1 <- xyplot(mpg~wt, data, type = "l")
  }
},
"ggplot2" = {
  for(i in 1:500){
    e2 <- ggplot(data = mtcars, aes(x = wt, y = mpg)) +
      geom_line()
  }
},
"graphics" = {
  for(i in 1:500){
    e3 <- plot(mtcars$wt, mtcars$mpg, type = "l")
  }
})

e1

e2

e3

autoplot(mbm_e)