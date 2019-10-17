do_stuff <- function() {
  for(i in 1L:100) {
    just_a_vector <- c(i, i^2, i^3)
    print(just_a_vector)
  }
}
  
do_stuff()


debug(do_stuff)
isdebugged(do_stuff)
undebug(do_stuff)
# where, n, s, f, c, Q
do_stuff()


