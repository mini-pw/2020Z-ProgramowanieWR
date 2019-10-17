library(SmarterPoland)
library(dplyr)
library(ggplot2)
library(lattice)

data_maths <- maturaExam %>% 
  filter(przedmiot == "matematyka")

# BOX_PLOTS
box_title <- "Rozklad punktow z matury z matematyki"
get_box_plot_ggplot <- function() {
  data_maths %>%
    ggplot(aes(x = rok, y = punkty)) +
    geom_boxplot() + 
    theme_classic() + 
    ggtitle(box_title)
}

get_box_plot_lattice <- function() {
  bwplot(punkty~rok, data_maths, main=box_title)
}

get_box_plot_graphics <- function() {
  boxplot(punkty~rok, data_maths, main=box_title)
}

# BARPLOTS
bar_title <- "Srednia punktow z matury z matematyki"
data_maths_mean <- data_maths %>% 
  group_by(rok) %>% 
  summarise(mean_points = mean(punkty)) %>% 
  as.data.frame()

get_bar_plot_ggplot <- function() {
  data_maths_mean %>% 
    ggplot(aes(x = rok, y = mean_points)) +
    geom_bar(stat="identity") +
    ggtitle(bar_title)
}

ylim_bar_plot <- c(0,max(data_maths_mean$mean_points)) * 1.05

get_bar_plot_lattice <- function() {
  barchart(mean_points ~ rok, data_maths_mean, main=bar_title, ylim=ylim_bar_plot)
}

get_bar_plot_graphics <- function() {
  plot(data_maths_mean$rok, data_maths_mean$mean_points, main=bar_title, type="h", ylim=ylim_bar_plot)
}

get_bar_plot_graphics()

# POINT
get_point_plot_ggplot <- function() {
  data_maths %>% 
    ggplot(aes(x = rok, y = punkty)) +
    geom_point()
}

get_point_plot_lattice <- function() {
  dotplot(punkty~rok, data_maths)
}

get_point_plot_graphics <- function() {
  plot(x=data_maths$rok, y=data_maths$punkty, type="p")
}

#HISTOGRAM

data_maths_2014 <- data_maths %>%
  filter(rok == 2014)

get_histogram_ggplot <- function() {
  data_maths_2014 %>% 
    ggplot(aes(x=punkty)) +
    geom_histogram(bins = 51) +
    theme_classic()
}

get_histogram_lattice <- function() {
  histogram(~punkty, data_maths_2014, type="count", breaks=0:50)
}

get_histogram_graphics <- function() {
  hist(data_maths_2014$punkty, breaks=0:50)
}

# DENSITY

get_density_plot_ggplot <- function() {
  data_maths_2014 %>% 
    ggplot(aes(x=punkty, fill=rok)) +
    geom_density(alpha=0.3)
}

get_density_plot_lattice <- function() {
  densityplot(~punkty, data=data_maths_2014, groups=rok, auto.key = TRUE)
}

get_density_plot_graphics <- function() {
  plot(density(data_maths_2014$punkty), main="")
}
