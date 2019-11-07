my_lm <- function(...) {
    time_start <- Sys.time()
    res <- list("model" = lm(...),
                "creation_date" = Sys.time(),
                "creation_time" = NULL,
                "comment" = NULL)
    class(res) <- "tm_class"
    
    res[["creation_time"]] <- Sys.time() - time_start
    res
}

is.tm_class <- function(x) {
    is_class_ok <- class(x) == "tm_class"
    are_names_ok <- setequal(names(x), c("model", "creation_date", "creation_time", "comment"))
    is_class_ok && are_names_ok
}

summary.tm_class <- function(object, ...) {
    stopifnot(is.tm_class(object))
    
    cat("tm_class attributes:\n")
    cat(sprintf("  - Model creation time: %0.6fs\n", object[["creation_time"]]))
    cat(sprintf("  - Model creation date: %s\n", object[["creation_date"]]))
    cat(paste0("  - Comment: ", object[["comment"]], "\n"))
    
    summary(object[["model"]], ...)
}

df <- data.frame(x = 1:3, y = (1:3)**2)
mlm <- my_lm(x ~ y, df)
mlm[["comment"]] <- "Hello world!"

summary(mlm)
