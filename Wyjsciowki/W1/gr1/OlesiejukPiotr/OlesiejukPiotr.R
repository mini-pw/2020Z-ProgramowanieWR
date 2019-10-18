student_object <- structure(c(id =9324), oceny = c(Matematyka = 5,
                                            Chemia = 4,
                                            Polski = 3,
                                            Fizyka = 5,
                                            Angielski = 3,
                                            Niemiecki = 4,
                                            Biologia = 5,
                                            Historia = 4,
                                            Wos = 3,
                                            Religia = 5), class = "student")

grades = c(Matematyka = 5,
          Chemia = 4,
          Polski = 3,
          Fizyka = 5,
          Angielski = 3,
          Niemiecki = 4,
          Biologia = 5,
          Historia = 4,
          Wos = 3,
          Religia = 5)


student <- function(id_num, marks) {
  obj <- structure(list(id = id_num, oceny = marks), class = "student")
  return(obj)
}

stud_obj <- student(20, grades)

mean.student <- function(student) {
  return(mean(attr(student_object, "oceny")))
}

mean.student(stud_object)

validator_student <- function(object) {
  n <- attr(object, "names")
  cl <- attr(object, "class")
  flag <- TRUE
  if (n[1] != "id" | n[2] != "oceny" | cl !="student") {
    flag <- FALSE
  }
  return(flag)
}

validator_student(stud_obj)
