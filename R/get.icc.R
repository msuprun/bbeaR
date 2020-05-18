
get.icc<-function(db){
  x<-db[,grep('rep.',colnames(db),fixed=T)]
  a<-icc(as.matrix(x))
  return(c(value=a$value, LCI=a$lbound, UCI=a$ubound))
}