#* @filter toupper
function(x = "") {
  res <<- list(x = toupper(x))
  
  forward()
}

#* @get /echo
function(x = "") {
  list(res[["x"]])
}
