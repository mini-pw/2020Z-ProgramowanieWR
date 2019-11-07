data <- data.frame(x = 1:10, y = runif(10))

adv_lm <- function(data, ...) 
{
  start  = Sys.time()
  time = system.time(model <- lm(data[,2]~data[,1]))
  my_obj <- structure(list(model = model, duration=time[1], comt = list(...) , creation_time = start ))
  class(my_obj) <- 'klasa'
  my_obj
}

validate_lm <- function(x)
{
  stopifnot(class(x$model) == 'lm')
  stopifnot(class(x$creation_time)[1] == "POSIXct" )
}

summary.klasa <- function(x)
{
  validate_lm(x)
  model <- x$model
  print(summary(model))
  cat(paste('Czas: ',x$creation_time, 'długosc tworzenia ', x$duration))
}

a <- adv_lm(data, 'komentarz')
summary(a)
