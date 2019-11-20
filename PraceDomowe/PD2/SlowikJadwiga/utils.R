library(dplyr)
library(rlang)

get_benchmark_df <- function(functions_list, data, ...) {
  names(functions_list) <- sapply(
    substitute(functions_list)[2:(length(functions_list) + 1)],
    as.character
  )
  
  dots <- enquos(...)
  c <- call2(
    microbenchmark::microbenchmark,
    !!!purrr::map(functions_list, function(f) {
      eval_tidy(expr(call2(f, data, !!!dots)))
    }),
    unit="ms",
    times=10
  )
  eval_tidy(c) %>% summary() %>% select(c("expr", "mean"))
}

