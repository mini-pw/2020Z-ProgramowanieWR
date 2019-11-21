
constructor <- function(..., comment=NULL) {
  invocation_date <- Sys.time()
  model <- lm(...)
  creation_date <- Sys.time()
  creation_time_elapsed <- difftime(creation_date, invocation_date)
  structure(list(model=model, creation_time_elapsed=creation_time_elapsed, creation_date=creation_date, comment=comment),
            class="lm_enhanced")
}

summary.lm_enhanced <- function(x, ...) {
  print(summary(x$model))
  print("additional fields")
  print(paste("creation_time_elapsed", x$creation_time_elapsed))
  print(paste("creation_date", x$creation_date))
  print(paste("comment", x$comment))
}

validate <- function(x) {
  if(all(c("model", "creation_time_elapsed", "creation_date", "comment") %in% names(x))) {
    TRUE
  } else {
    FALSE
  }
}

# USAGE

use_lm_enhanced <- function(df, comment = NULL) {
  lmEnhancedObject <- constructor(x ~ y, df, comment=comment)
  print(lmEnhancedObject)
  summary(lmEnhancedObject)
  validationResult <- validate(lmEnhancedObject)
  print(validationResult)
}

use_lm_enhanced(data.frame(x=1:10, y=1:10), "my comment")
use_lm_enhanced(data.frame(x=c(1,2,3,4), y=c(3,8,3,5)))


