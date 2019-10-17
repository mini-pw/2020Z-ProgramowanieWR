#' @title Hello function
#' 
#' @param txt \code{character} vector
#' @details More informations.
#' @seealso \code{\link[base]{cat}}
#' @author Michal
#' @export
#' @examples 
#' # random comment
#' hello("Michal")
hello <- function(txt = "world") {
    cat("Hello, ", txt, "\n")
}
