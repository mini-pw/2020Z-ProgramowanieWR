library(rlang)

my_NSE <- function(fun) {
  function(data, ...) {
    print("nse")
    print(data)
    dots <- enquos(...)
    args <- lapply(dots, function(arg) {
      eval_tidy(arg, data=data)
    })
    exec(fun, !!!args)
  }
}

my_NSE2 <- function(fun) {
  function(data, ...) {
    print("nse2")
    print(data)
    dots <- exprs(...)
    args <- purrr::map(dots, function(arg) eval_tidy(arg, data))
    eval_tidy(expr(fun(!!!args)))
  }
}

my_NSE3 <- function(fun) {
  function(data, ...) {
    print("nse3")
    print(data)
    dots <- enquos(...)
    args <- purrr::map(dots, function(arg) eval_tidy(arg, data))
    eval_tidy(call2(expr(fun), !!!args))
  }
}

my_NSE4 <- function(fun) {
  function(data, ...) {
    print("nse4")
    print(data)
    dots <- as.list(substitute(list(...)))[-1]
    args <- purrr::map(dots, function(arg) eval(arg, data))
    do.call(fun, args)
  }
}
