NSE_factory <- function(FUNCTION){
  out <- function(argslist, ...){
    given_args <- quos(...)
    lapply(given_args, function(expr) eval(quo_squash(expr), argslist)) -> evaluated_args
    do.call(FUNCTION, evaluated_args)
  }
}
