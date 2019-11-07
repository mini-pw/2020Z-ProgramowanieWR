#  ellipsis in R

length_m <- function(...) {
  lengths(list(...))
}

length_m(c(1, 2), c(1:5), c(2:100), c(1:2))

x <- list(c(1, 2), c(1:5), c(2:100))
length_m(x)
do.call(length_m, x)

1L:5

# S3 -----------------------------

methods(print)

# x <- matrix(rnorm(20), nrow = 5)
# class(x)
# str(x)
# attributes(x)
# storage.mode(x) <- "integer"
# class(x)
# attributes(x)


example_object1 <- structure(c("A", "B", "C"), class = "example_class")

str(example_object1)

example_object2 <- c("A", "B", "C")
class(example_object2) <- "example_class"

str(example_object2)

example_object3 <- c("A", "B", "C")
attr(example_object3, "class") <- "example_class"

str(example_object3)

identical(example_object1, example_object2)
identical(example_object1, example_object3)
identical(example_object2, example_object3)

example_factor <- factor(letters[1L:3])
class(example_factor) <- "example_class"

print.example_class <- function(x, ...) {
  cat("Letters:\n")
  cat(x)
}

plot.example_class(x, y, bar_color = "red", ...)

print(example_object1)

summary.example_class <- function(object, ...) {
  summ <- table(factor(object, levels = letters))
  cat("Counts of letters:\n")
  summ
}

summary(example_object1)
methods(summary)

# zadanie: stwórz obiekt z dwoma klasami. Porównaj działanie UseMethod i NextMethod.

# S4 ------------------------------------------
library(methods)

setClass("example_class", 
         slots = c(value = "character"),
         prototype = list(value = NA_character_))

example_object <- new("example_class", value = c("A", "B", "C"))

setMethod("as.character", "example_class", function(x) slot(x, "value"))
as.character(example_object)

# zadanie: stwórz obiekt z dwoma klasami. Pokaż jak działa multiple dispatch
