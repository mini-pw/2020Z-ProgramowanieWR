#' @title Hello  function
#' 
#' @param txt an object of \code{character} class
#' @importFrom dplyr %>%
#' @export
#' @details Here explain what function is doing and why.
#' @return \code{NULL} (invisible)
#' @seealso \code{\link[base]{debug}}
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
