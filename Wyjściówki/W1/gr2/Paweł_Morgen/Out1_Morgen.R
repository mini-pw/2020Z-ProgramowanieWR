#Brudnopis:
new_matrix<-function(..., commentToData=NA_character_){
  new_data<-list(...)
  system.time(x<-do.call(matrix,new_data))->t1
  t2<-date()
  structure(x, CreationDate=t2, CreationTime=t1[3], comment=commentToData)
}
t2-t1
typeof(t1)
new_matrix(commentToData = "Success!", 1:100, 10, 10)

#Czystopis:
new_lm<-function(..., comment_to_data=NA_character_){
  new_data<-list(...)
  system.time(x<-do.call(lm,new_data))->t1
  t2<-date()
  structure(x, class="new_lm", CreationDate=t2, CreationTime=t1[3], comment=comment_to_data)
}
is.new_lm(x,...){
  
  
  
}
summary.new_lm<-function(x, ...){
  cat(summary(x))
  cat("CreationTime:\n")
  cat(x$CreationDate)
  cat("CreationDate:\n")
  cat(x$CreationTime)
}