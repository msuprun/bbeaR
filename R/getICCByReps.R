
getICCByReps<-function(eset, unit.reps.vars, type, model){
  require(irr)
  eset$unit.factor<-drop.levels(factor(apply(pData(eset)[,unit.reps.vars],1,paste,collapse='_')))
  icc.a<-(sapply(levels(eset$unit.factor), function(l,mat,pd){
    a<-icc(mat[,which(pd$UnitRep==l)], type=type, model=model)
    return(c(a$value, a$lbound, a$ubound))
  }, mat=exprs(eset), pd=pData(eset),simplify = TRUE, USE.NAMES = TRUE))
  pd_m<-as.data.frame(t(sapply(colnames(icc.a), function(x){unlist(strsplit(x,'.',fixed=T))})))
  icc.a<-cbind.data.frame(t(icc.a),pd_m)
  #colnames(icc.a)<-c('ICC.A','LCI','UCI','Group','SUBJECT.ID','VISIT');
  colnames(icc.a)<-c('ICC','LCI','UCI',colnames(pd_m));
  return(icc.a)
}