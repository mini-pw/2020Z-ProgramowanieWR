baseR_nse <- function(func) {
  function(data, ...) {
    args <- as.list(substitute(...()))
    env <- list2env(data)
    browser()
    do.call(func, args, envir=env)
  }
}

tidy_nse <- function(f) {
  function(data, ...) {
    args <- exprs(...)
    ef <- enexpr(f)
    eval_tidy(expr((!!ef)(!!!args)), data=data)
    
  }
} 

data("Satellite" )

small_list <- list(a=c(1,2,3,4), b=c(6,3,5))
df <- Satellite
lex <- list(a = list(1:5, LETTERS[1:5]), b = "Z", c = NA)

test_function <- function(nse_factory, delegate, ...) {
  nse_delegate <- nse_factory(delegate)
  nse_delegate(...)
}

test_function(baseR_nse, min, small_list, b)
test_function(baseR_nse, min, data=df, x.1~x.2, weights=x.5)

lm(x.1~x.2, df, weights = df[['x.5']])
lm_nse <- baseR_nse(lm)
lm_nse(df, x.1~x.2, weights=x.5)

base_min <- function() {
  min
}

baseR_min_nse <- function() {
  min_nse_baseR <- baseR_nse(min)
  min_nse_baseR(small_list, a)
}

  min_nse_baseR(df, x.2)
baseR_mean_nse <- function() {
  mean_nse_baseR <- baseR_nse(mean)
  mean_nse_baseR(small_list, a)
  mean_nse_baseR(df, x.2)
}

baseR_lm_nse <- function() {
  lm(x.1~x.2, df, weights = df[['x.5']])
  lm_nse <- baseR_nse(lm)
  lm_nse(df, x.1~x.2, weights=x.5)
}

tidy_lm_nse <- function() {
  lm_nse <- tidy_nse(lm)
  lm_nse(df, x.1~x.2, weights=x.5)
}

unlist(lex, use.names = FALSE)

unlist_nse <- tidy_nse(unlist)
unlist_nse(data.frame(lex=lex,o=c(1:5)), lex, use.names=FALSE)
unlist_nse <- nse_substitute(unlist)
unlist_nse(data.frame(lex=lex,o=c(1:5)), lex, use.names=FALSE)
unlist_nse(data.frame(lex=lex,o=c(1:5)), lex, use.names=TRUE)
unlist(lex, use.names = TRUE)
unlist_nse(data.frame(lex=lex,o=c(1:5)), lex, use.names=TRUE)


           