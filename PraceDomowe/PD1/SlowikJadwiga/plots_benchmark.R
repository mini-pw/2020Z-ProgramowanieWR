library(dplyr)
source("plots_generator.R")

get_benchmark_result <- function(ggplot_fun, lattice_fun, graphics_fun) {
  box_benchmark_result <- microbenchmark::microbenchmark(
    ggplot=ggplot_fun(),
    lattice=lattice_fun(),
    graphics=graphics_fun()
  ) %>%
    summary() %>% 
    select("expr", "mean", "median")
}

compute_and_save_benchmark_result <- function(file_name, ggplot_fun, lattice_fun, graphics_fun) {
  r <- get_benchmark_result(ggplot_fun=ggplot_fun, lattice_fun=lattice_fun, graphics_fun=graphics_fun)
  write.csv(r, file_name)
}

# computing and saving benchmarks for each test triple
compute_and_save_benchmark_result(
  file_name="benchmark_results/box_plot.csv",
  ggplot_fun=get_box_plot_ggplot,
  lattice_fun=get_box_plot_lattice,
  graphics_fun=get_box_plot_graphics
)

compute_and_save_benchmark_result(
  file_name="benchmark_results/bar_plot.csv",
  ggplot_fun=get_bar_plot_ggplot,
  lattice_fun=get_bar_plot_lattice,
  graphics_fun=get_bar_plot_graphics
)

compute_and_save_benchmark_result(
  file_name="benchmark_results/point_plot.csv",
  ggplot_fun=get_point_plot_ggplot,
  lattice_fun=get_point_plot_lattice,
  graphics_fun=get_point_plot_graphics
)

compute_and_save_benchmark_result(
  file_name="benchmark_results/histogram.csv",
  ggplot_fun=get_histogram_ggplot,
  lattice_fun=get_histogram_lattice,
  graphics_fun=get_histogram_graphics
)

compute_and_save_benchmark_result(
  file_name="benchmark_results/density_plot.csv",
  ggplot_fun=get_density_plot_ggplot,
  lattice_fun=get_density_plot_lattice,
  graphics_fun=get_density_plot_graphics
)
