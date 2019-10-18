create_student <- function(studnet, oceny) {
 
  if (round(studnet)!=studnet | studnet < 0 ) {
    stop("student has to be an numeric")
  } 
  if (!is.numeric(oceny)) {
    stop("oceny has to be a numeric")
  }
  if (length(names(oceny)) != length(oceny)) {
    stop("oceny has to be a named vector")
  }
  if (!all(oceny <= 6 & oceny >=1)) {
    stop("Oceny has to be lower or equal 6 and higher or equal than 1")
  }
  student_object <- list(id = studnet, oceny = oceny)
  class(student_object) <- "student"
  student_object
}


is_student <- function(student) {
 if (length(student) != 2) {
   return(FALSE)
 }
 if (names(student)[1] != "id" | names(student)[2] != "oceny") {
   return(FALSE)
 } 
 if (floor(student[["id"]])!=student[["id"]] | student[["id"]] < 0  | !is.numeric(student[["oceny"]])) {
   return(FALSE)
 } 
 if (!all(student[["oceny"]] <= 6 & student[["oceny"]] >=1)) {
   return(FALSE)
 }
 TRUE
}


mean.student <- function(x, ...) {
  mean(x[["oceny"]])
}

