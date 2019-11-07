constr.student <- function(id = "Jane Doe", oceny = rep(2,10), ...){
  s <- structure(list(id,oceny), class = "student")
  s
}

valid.student <- function(s, ...){
  nazwy <- c('p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10')
  stopifnot(is.character(s[[1]]))
  stopifnot(length(s[[2]]) == 10)
  stopifnot(class(o) == "numeric")
  stopifnot( ( sort( names(s[[2]]) ) == sort(nazwy)))
}


o <- c(rep(4,5), rep(4.5,5))
names(o) <- c('p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10')

HZ <- constr.student("Hanna Zdulska", o) 
valid.student(HZ)

mean.student <- function(s, ...){
  mean(s[[2]])
}

mean.student(HZ)
