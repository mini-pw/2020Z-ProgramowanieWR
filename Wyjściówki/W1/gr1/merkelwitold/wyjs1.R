# Zakladam ze oceny studentow sa z przedzialu [1, 10] i sa naturalne

constructor <- function(ID = 291135, oceny = c(1:10)){
  stopifnot(is.numeric(ID) & is.numeric(oceny) & ID > 0)
  structure(list(oceny = oceny, ID = ID), class = "student")
}

validator <- function(student){
  stopifnot(class(student) == "student")
  stopifnot(is.numeric(ID) & is.numeric(oceny) & ID > 0 & oceny > 0 & oceny < 11)
  TRUE
}

mean.student <- function(x, ...){
  mean(student$oceny)
}

student <- constructor(1234123, c(5,1,7,9,2,5,7,8,9,10))