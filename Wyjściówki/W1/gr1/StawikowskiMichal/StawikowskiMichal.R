v <- c(p1=5,p2=3,p3=2,p4=5,p5=2,p6=2,p7=4,p8=3,p9=2,p10=10)


Student <- structure(list(oceny = c(p1=5,p2=3,p3=2,p4=5,p5=2,p6=2,p7=4,p8=3,p9=2,p10=10), id=5), class = 'student')


mean.student <- function(x,...){
  stopifnot(is.vector(x$oceny))
  mean(x$oceny)
}


Student_validator <- function(Student){
  stopifnot(class(Student) == 'student')
  stopifnot(is.numeric(Student$id) & is.vector(Student$oceny) & Student$id > 0)
  TRUE
}

Student_constructor <- function(StudentId = 0, ocenyStudenta = rep(0,10)) {
  stopifnot(is.numeric(StudentId) & is.vector(oceny) & StudentId > 0)
  structure(list(oceny = oceny, id = StudentId), class = 'student')
}

