get.cor<-function(db,m){
  x<-db[,grep('rep.',colnames(db),fixed=T)]
  x<-as.matrix(x)
  a<-cor.test(x[,1],x[,2])
  return(c(value=a$estimate, LCI=a$conf.int[1], UCI=a$conf.int[2]))
}
