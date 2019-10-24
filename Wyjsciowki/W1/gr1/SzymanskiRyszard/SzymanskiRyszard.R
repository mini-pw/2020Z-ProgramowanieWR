library(magrittr)

create_student <- function(id, grades) {
  structure(
    list(
      id = id,
      grades = grades
    ),
    class = "student"
  )  
}

validate_student <- function(student) {
  is_intiger <- function(x) {
    student$id %% 1 == 0
  }
  
  check_grades <- function(grades) {
    stopifnot(is.vector(student$grades) && !is.list(student$grades))
    stopifnot(!is.null(names(student$grades)))
  }
  
  stopifnot(class(student) == "student")
  stopifnot(all(c("id", "grades") %in% names(student))) # check if it contains required fields
  stopifnot(is_intiger(student$id)) # check if it is an integer
  check_grades(student$grades)
}


mean.student <- function(x, ...) {
  validate_student(x)
  mean(x$grades)
}

example_student <- create_student(
  id = 1,
  grades = c(
    Mathematics = 5,
    English = 4,
    Biology = 4,
    Physics = 5,
    Chemistry = 3,
    PE = 5,
    Spanish = 4,
    Geography = 5,
    French = 3,
    German = 4
  )
)

mean(example_student)
