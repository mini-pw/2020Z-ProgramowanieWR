

student1 <- structure(c(id="A",grades=c(p1=5,p2=3,p3=4,p4=4,p5=5,p6=3,p7=3,p8=4,p9=5,p10=5)), class = "student_class")

mean.student <- function(x,...){
  y <- unclass(x)
  oceny <- as.numeric(y[-1])
  mean(oceny)
}

create.student <- function(studentId, oceny){
  stopifnot(is.character(studentId))
  stopifnot(is.numeric(oceny))
  stopifnot(length(oceny)==10)
  stopifnot(length(studentId)==1)
  x <- structure(c(id=studentId, grades=oceny), class = "student_class")
  x
}

validate.student <- function(student){
  if(class(student)!="student_class") 0
  y <- unclass(student)
  if(!is.character(y[1])) 0
  if(!is.numeric(y[-1])) 0
  if(length(y[-1])>10) 0
  if(any(as.numeric(y[2:10])<2)) 0
  if(any(as.numeric(y[2:10])>2)) 0
  1
}



