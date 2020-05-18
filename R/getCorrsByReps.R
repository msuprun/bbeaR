
getCorrsByReps<-function(eset, pd, unit.factor,method='pearson'){
  uf<-factor(pData(eset)[,unit.factor])
  mc<-(sapply(levels(uf), function(l,mat,pd){
    a<-cor(mat[,which(pd$UnitRep==l)],use='p',method=method)
    return(mean(a[lower.tri(a)]))
  },  exprs(eset), pData(eset),
  simplify = TRUE, USE.NAMES = TRUE))
  pd_m<-as.data.frame(t(sapply(names(mc), function(x){unlist(strsplit(x,'.',fixed=T))})))
  out<-cbind.data.frame((mc),pd_m)
  colnames(out)<-c('Cor','Group','SUBJECT.ID','VISIT');
  return(out)
}