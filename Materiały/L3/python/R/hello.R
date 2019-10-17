#' @title Hello function
#' 
#' @param txt \code{character} vector
#' @details More informations.
#' @seealso \code{\link[base]{cat}}
#' @author Michal
#' @export
hello <- function(txt = "world") {
    cat("Hello, ", txt, "\n")
}
