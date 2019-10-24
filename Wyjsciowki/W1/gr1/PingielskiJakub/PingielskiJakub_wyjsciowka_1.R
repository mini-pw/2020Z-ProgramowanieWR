construct_student <- function(ID, oceny){
  
  student <- structure(list("ID" = as.integer(ID), "oceny" = oceny), class = 'student')
  
  student
}



mean.student <- function(student){
  mean(ja$oceny)
}

validate.student <- function(student){
  stopifnot(is.integer(student$ID))
  stopifnot(is.vector(student$oceny))
  stopifnot(!is.null(is.integer(student$oceny)))
  stopifnot(names(student$oceny))
  stopifnot(length(student$oceny != 10))
}

oceny <- c(1:10)
names(oceny) <- c('p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10')

ja <- construct_student(23, oceny)

mean.student(ja)

validate.student(ja)


