is.grade <- function(x, possible_grades) {
  return(is.numeric(x) & x %in% possible_grades)
}

construct_student <- function(id, grades, possible_grades = c(2, 3, 3.5, 4, 4.5, 5), ...) {
  # grades must be numeric (because of mean)
  stopifnot(is.numeric(possible_grades))
  stopifnot(validate_student(id, grades))
  return(structure(list(id = id, grades = grades), class = "student"))
}

validate_student <- function(id, grades, possible_grades) {
  if (is.numeric(id) && length(id) == 1 && is.numeric(grades) && length(names(grades)) == length(grades) &&
      length(grades) == 10 && all(is.grade(grades, possible_grades))) {
    return(TRUE)
  }
  return(FALSE)
}

mean.student <- function(x, ...) {
  mean(x$grades)
}

example_student <- construct_student(1972832,
                                     c(Maths = 4, More_Maths = 4.5, History_of_Maths = 3, Math_of_History = 3.5,
                                       Maths_for_Experienced = 5, Maths_for_Dummies = 2, Maths_in_R = 4.5,
                                       Maths_out_of_R = 3, Maths_in_And_out = 4, Maths_All_Year_Round = 2))
