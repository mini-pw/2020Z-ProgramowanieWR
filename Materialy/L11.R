library(plumber)

srv <- plumb("./Materialy/L11/auth.R")

srv[["run"]](port=8080)

srv2 <- plumb("./Materialy/L11/toupper.R")
srv2[["run"]](port=8080)
