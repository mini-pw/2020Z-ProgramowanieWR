#' @title Hello  function
#' 
#' @param txt an object of \code{character} class
#' @importFrom dplyr %>%
#' @export
#' @return \code{NULL} (invisible)
#' @examples
#' hello("Michal")
hello <- function(txt = "world") {
  paste0("Hello, ", txt) %>% 
    cat("\n")
  invisible(NULL)
}



#' @title Hello/witam  function
#' 
#' @inheritParams hello
#' @export
hello_witam <- function(txt = "world") {
  paste0("Hello/Witam, ", txt) %>% 
    cat("\n")
}
