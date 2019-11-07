
construct_modelPlus <- function(..., comment = NULL) {
  
  args <- list(...)
  model <- lm(args)
  
  if (!validate_modelPlus(comment)) return(NULL)
  
  ret <- list()
  
  ret[['model']] <- model
  ret[['czas_tworzenia']] <- system.time({ lm(args) })
  ret[['data_utworzenia']] <- Sys.Date()
  ret[['comment']] <- comment
  
  class(ret) <- "modelPlus"
  ret
}

validate_modelPlus <- function(comment) {
  
  if (is.null(comment)) return(TRUE)
  
  if (!is.character(comment)) return(FALSE)
    
  if (length(comment) > 1) return(FALSE)
  
  TRUE
}

summary.modelPlus <- function(object, ...) {
  
  ret <- object
  summ <- summary(object[['model']])
  ret <- list(object, summ)
  
  print(ret)
}

x1 <- 21:40
y <- 1:20

super_model <- construct_modelPlus(formula = as.formula(y ~ x1), comment = "Best model EVER!")
summary(super_model)

super_model2 <- construct_modelPlus(formula = as.formula(y ~ x1))
summary(super_model2)

super_model3 <- construct_modelPlus(formula = as.formula(y ~ x1),
                                    data = data.frame(y = rnorm(200), x1 = runif(200)),
                                    comment = "Better model!")
summary(super_model3)

# testy walidatora
testthat::expect_null(construct_modelPlus(formula = as.formula(y ~ x1), comment = c("a", "b")))
testthat::expect_null(construct_modelPlus(formula = as.formula(y ~ x1), comment = 1))
