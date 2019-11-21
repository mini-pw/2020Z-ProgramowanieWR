makeNseFunction1 <- function(fun) {
  function(data, elementOrFormula, ...) {
    functionEnvironment = environment()
    
    if (as.character(substitute(elementOrFormula)) %in% names(data)) {
      argument = data[[deparse(substitute(elementOrFormula))]]
    } else {
      allVariables = all.vars(elementOrFormula)
      for (variable in allVariables) {
        assign(variable, eval(as.name(variable), data), envir = functionEnvironment)
      }
      argument = elementOrFormula
      environment(argument) = functionEnvironment
    }
    fun(argument, ...)
  }
}

makeNseFunction2 <- function(fun) {
  function(data, elementOrFormula, ...) {
    library(rlang)
    functionEnvironment = environment()
    
    if (as.character(substitute(elementOrFormula)) %in% names(data)) {
      elementOrFormula = substitute(elementOrFormula)
      argument = eval(enexpr(elementOrFormula), data)
    } else {
      allVariables = all.vars(elementOrFormula)
      for (variable in allVariables) {
        assign(variable, eval(as.name(variable), data))
      }
      argument = elementOrFormula
      environment(argument) = functionEnvironment
    }
    fun(argument, ...)
  }
}

makeNseFunction3 <- function(fun) {
  function(data, elementOrFormula, ...) {
    functionEnvironment = environment()
    
    if (as.character(substitute(elementOrFormula)) %in% names(data)) {
      argument = eval(substitute(elementOrFormula), data)
    } else {
      allVariables = all.vars(elementOrFormula)
      for (variable in allVariables) {
        assign(variable, eval(as.name(variable), data))
      }
      argument = elementOrFormula
      environment(argument) = functionEnvironment
    }
    fun(argument, ...)
  }
}

# Development tests ====
x <- list(a = c(1, 2, 4), b = c(2, 1))
min_NSE <- makeNseFunction2(min)
min_NSE(x, a)

x <- list(a = c(1, 2, 4), b = c(2, 1))
mean_NSE <- makeNseFunction(mean)
mean_NSE(x, a, use.names = FALSE)

x <- list(a = list(x = 1, y = 2, z = 4), b = list(u = 2, w = 1))
unlist_NSE <- makeNseFunction2(unlist)
unlist_NSE(x, a, use.names = F)
unlist_NSE(x, a, use.names = T)

data = data.frame(x = c(1, 2, 3), y = c(1, 4, 9))
lm_NSE <- makeNseFunction2(lm)
lm_NSE(data = data, elementOrFormula = x ~ y)

# Efficiency tests ====
library(microbenchmark)
library(ggplot2)

results = data.frame()
testedFunction = min
datasetSize = 100
dataset = list(a = sample(x = 1:100, size = datasetSize))
functionNse <- makeNseFunction1(testedFunction)
results = rbind(results, microbenchmark(
  function1 = functionNse(data = dataset, a)
))
functionNse <- makeNseFunction2(testedFunction)
results = rbind(results, microbenchmark(
  function2 = functionNse(data = dataset, a)
))
functionNse <- makeNseFunction3(testedFunction)
results = rbind(results, microbenchmark(
  function3 = functionNse(data = dataset, a)
))

summary(results)

ggplot(data = summary(results), aes(x = expr, y = mean, fill = expr))+
  geom_bar(stat = "identity") + 
  geom_text(aes(x=expr,y=mean,label=mean),vjust=-.5) +
  xlab("Function") + 
  ylab("Mean execution time [ms]") +
  scale_fill_manual(values = c("#1D3461", "#BC9EC1", "#D282A6")) +
  theme_minimal() +
  theme(legend.position = "none")



testedFunction = lm
datasetSize = 100
dataset = list(x = sample(x = 1:100, size = datasetSize), y = sample(x = 1:100, size = datasetSize))
formula = x ~ y 

# Efficiency tests ====
library(microbenchmark)
library(ggplot2)

results = data.frame()
functionNse <- makeNseFunction1(testedFunction)
results = rbind(results, microbenchmark(
  function1 = functionNse(data = dataset, formula)
))
functionNse <- makeNseFunction2(testedFunction)
results = rbind(results, microbenchmark(
  function2 = functionNse(data = dataset, formula)
))
functionNse <- makeNseFunction3(testedFunction)
results = rbind(results, microbenchmark(
  function3 = functionNse(data = dataset, formula)
))

ggplot(data = summary(results), aes(x = expr, y = mean, fill = expr))+
  geom_bar(stat = "identity") + 
  geom_text(aes(x=expr,y=mean,label=mean),vjust=-.5) +
  xlab("Function") + 
  ylab("Mean execution time [ms]") +
  scale_fill_manual(values = c("#1D3461", "#BC9EC1", "#D282A6")) +
  theme_minimal() +
  theme(legend.position = "none")

