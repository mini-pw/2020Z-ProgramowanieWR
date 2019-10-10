#Utwórz klasę S3 dla obiektów klasy lm, która oprócz samego modelu zawiera 
#również czas jego tworzenia, datę jego utworzenia oraz opcjonalne pole na 
#string z komentarzem. Dla tej klasy przygotuj funkcję summary (która powinna 
#działać tak jak summary.lm, ale ma dodatkowo pokazywać dodatkowe pola tego obiektu) 
#oraz konstruktor i walidator.


new_lm <- function(...) {
  structure(
    data_utw <- Sys.time(),
    model <-lm(...),
    czas_przetwarzania <- (Sys.time()-data_utw),
    komentarz <- NULL, class = "new_lm_"
  )
}

summary.new_lm_ <- function(object, ...){
  summary.object$model
  print(paste0("data_utworzenia: ", object$data_utw))
  print(paste0("data_tworzenia: ", object$czas_przetwarzania))
  print(paste0("komentarz: ", object$komentarz))
}

#walidator
is.new_lm_<-function(object){
  class(object)=="new_lm_"
}


  
df<- data.frame(x = 1:4,y = 5:8)
test<- new_lm(x~y, df)
