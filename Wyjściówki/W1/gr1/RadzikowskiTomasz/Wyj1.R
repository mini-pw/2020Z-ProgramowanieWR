st1 <- structure(student_id="0000", oceny=c(p1=5,p2=5,p3=5,p4=5,p5=5,p6=5,p7=5,p8=5,p9=5,p10=5), class = "student")
st1<-list(student_id="0000", oceny=c(p1=5,p2=5,p3=5,p4=5,p5=5,p6=5,p7=5,p8=5,p9=5,p10=5))
class(st1)<-"student"



students_con<- function(id, wektor_ocen) {
value<-list(student_id=id, oceny=wektor_ocen)
attr(value, "class")<-"student"
value
}



stt<-students_con(101,c(5,2,2,2,2,2,2,2,2,2))


mean.student<-function(obj,...) {
  cat("mean:", mean(obj$oceny))
}


mean(stt)
#stt$student_id

validator<-function(obj) {
  if (typeof(obj$student_id)!="character") {
    return (FALSE)
  }
  if (typeof(obj$student_id)=="character" && class(obj)=="student" && length(obj$oceny)==10&& all(obj$oceny>=2)&&all(obj$oceny<=5)) {
    return (TRUE)
  }
  if (length(obj$oceny)!=10 || all(obj$oceny<=2) || all(obj$oceny>=5)) {
    return (FALSE)
  }
}


