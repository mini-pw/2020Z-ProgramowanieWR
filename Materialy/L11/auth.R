usr_v <- c("Jan", "Dominik", "Mateusz", "Paulina")

#* @filter auth
function(x = "") {
  if(!(x %in% usr_v))
    stop("Blad")
  
  forward()
}


#* @get /echo
function(x = "") {
  list(res = paste0("User: ", x))
}
