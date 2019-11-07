id <- 298712

marks <- c(zaawansowany_R = 5, 
           bioinformatyka = 5, 
           procesy_stochastyczne = 5, 
           od_html_do_postgis = 4.5,
           projekt_interdyscyplinarny = 3.5,
           fizyka = 3,
           podstawy_metamatematyki_i_teorii_obliczalnosci = 5,
           metody_optymalizacji = 3.5,
           metody_statystyki_obliczeniowej = 4.5,
           randomowy_przedmiot = 3)

construct_student <- function(id, marks) {
  if (!is.numeric(id) || id < 0 || floor(id) != id || length(id) != 1) stop("id has to be a single positive integer number")
  if (!is.numeric(marks) || is.null(names(marks))) stop("marks has to be a named numeric vector")
  if (!all(marks  %in% c(2, 3, 3.5, 4, 4.5, 5))) stop("all possible marks are 2, 3, 3.5, 4, 4.5, 5")
  ret <- list(id = id, marks = marks)
  class(ret) <- "student"
  ret
}

validate_student <- function(student) {
  id <- student[["id"]]
  marks <- student[["marks"]]
  if (class(student) != "student") return(FALSE)
  if (is.null(id) || is.null(marks)) return(FALSE)
  if (!is.numeric(id) || id < 0 || floor(id) != id || length(id) != 1) return(FALSE)
  if (!is.numeric(marks) || is.null(names(marks))) return(FALSE)
  if (!all(marks  %in% c(2, 3, 3.5, 4, 4.5, 5))) return(FALSE)
  return(TRUE)
}

mean.student <- function(x, trim, na.rm, ...) {
  mean(x[["marks"]])
}

print.student <- function(x, ...) {
  cat("Czesc!\nJestem wesolym studentem MiNI!\nOto moje oceny:\n", paste(names(x[["marks"]]), ":", x[["marks"]], sep = "", collapse = "\n"))
}

Billy <- construct_student(id, marks)
validate_student(Billy)
mean(Billy)
Billy
