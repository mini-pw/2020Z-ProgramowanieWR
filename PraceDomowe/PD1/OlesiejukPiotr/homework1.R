library(lattice)
library(ggplot2)
library(graphics)
library(magrittr)

#setwd("~/Studia IAD/R for Advanced")

measure_time <- function(job, n = 20) {
  s = 0
  for (i in 1:n) {
  starting <- Sys.time()
  eval(job)
  ending <- Sys.time()
  t <- ending - starting
  s = s + as.numeric(t)
  }
  print(paste0("Time elapsed: ", round(s / n, 5), " seconds"))
  return(s / n)
}

wide_vec <- function(vec, k) {
  result <- vector(mode = "character", length = k*length(vec))
  j <- 1
  for (i in seq(1, length(result), by = k)) {
    result[i:(i+k - 1)] <- rep(vec[j], k)
    j <- j + 1
  }
  result
}

## Domyœlny barplot

df <- USArrests[order(USArrests$Murder, decreasing = TRUE)[1:10], 1]
df <- data.frame(States = rownames(USArrests)[1:10], Murders = df)

t_bar_gg <- measure_time(
  { print(ggplot(df, aes(x = States, y = Murders)) + geom_col()) }
)

t_bar_latt <- measure_time(
  { print(barchart(Murders ~ States, data = df)) }
)

t_bar_base <- measure_time(
  { barplot(df$Murders, names.arg = df$States) }
)


### Scatterplot

df <- iris

#ggplot2
t_scat_gg <-  measure_time(
  { print(ggplot(data = df, aes(Sepal.Length, Sepal.Width)) + geom_point()) }
)

#base
t_scat_base <- measure_time(
  { plot(df$Sepal.Length, df$Sepal.Width) }
)

#lattice
t_scat_latt <- measure_time(
  { print(xyplot(Sepal.Width ~ Sepal.Length, data = df)) }
)

### Boxplot

df <- datasets::PlantGrowth

t_box_gg <- measure_time(
  { print(ggplot(data = df, aes(x = group, y = weight)) + geom_boxplot()) }
)

t_box_base <- measure_time(
  { boxplot(weight ~ group, data = df) }
)

t_box_latt <- measure_time(
  { print(bwplot(weight ~ group, data = df)) }
)


### Piechart

library(dplyr)



df <- as.data.frame(datasets::Titanic)
df <- df %>% group_by(Class) %>% summarise(People =sum(Freq)) %>% data.frame

t_pie_gg <- measure_time(
   { print(ggplot(dat = df, aes(x = "", y = People,fill = Class)) + geom_bar(width = 1, stat = 'identity') +
    coord_polar(theta = 'y', start = 0)) }
)

t_pie_base <- measure_time(
  { pie(x = df$People, labels = df$Class) }
)

t_pie_latt <- 0 # nie ma w lattice piechartow

### Facets

t_fac_gg <- measure_time({
  print(ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point() +
    facet_wrap(~Species)) })

t_fac_latt <- measure_time(
  { print(xyplot(Sepal.Width ~ Sepal.Length | Species, data = iris)) })

t_fac_base <- measure_time(
  {
    df <- split(iris, iris$Species)
    par(mfrow = c(2,2))
    lapply(df, function(d) plot(Sepal.Width ~ Sepal.Length, data = d))
  }
)

results <- data.frame(Package = rep(c("ggplot2", "lattice", "graphics"), 5),
                      Plot = wide_vec(c("Barplot", "Scatterplot", "Boxplot", "Piechart", "Facet"), k = 3),
                      Time = c(t_bar_gg, t_bar_latt, t_bar_base, t_scat_gg, t_scat_latt, t_scat_base,
                               t_box_gg, t_box_latt, t_box_base, t_pie_gg, t_pie_latt, t_pie_base,
                               t_fac_gg, t_fac_latt, t_fac_base) * 1000
                      )

print(ggplot(data = results, aes(x = Package, y = Time, fill = Package)) + geom_col() + 
  facet_wrap(~Plot) +
  ggtitle("Execution time in miliseconds") +
  theme_fivethirtyeight() + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        plot.background = element_blank()))


