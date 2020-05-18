getAnalyte.general <- function(eset,unit.reps.vars){
  require(dplyr)
  eset$unit.factor<-drop.levels(factor(apply(pData(eset)[,unit.reps.vars],1,paste,collapse='_')))
  getme <- lapply(featureNames(eset), function(l){
    A.merge<-arrange(cbind.data.frame(
      data.frame(Outcome=t(exprs(eset[l,]))[,1]),   Analyte=l, UnitRep=eset$unit.factor),
      UnitRep)
    A.count <- data.frame(A.merge %>% group_by(UnitRep) %>% mutate(counter=row_number(UnitRep)))
    A.reshape <-arrange(dcast(A.count, Analyte + UnitRep ~ counter, value.var="Outcome"),Analyte)
    return(A.reshape)
  })
  Analyte.Data <-do.call('rbind',  getme)
  wc<-(colSums(!is.na(Analyte.Data[,-c(1:2)]))>nrow(Analyte.Data)*0.4)
  wout<-names(wc)[!wc]
  Analyte.Data<-merge(Analyte.Data[,!colnames(Analyte.Data)%in%wout],
                      pData(featureData(eset)), by='Analyte',all.x=TRUE)
  cn<-colnames(Analyte.Data)
  cn[(cn%in%as.character(1:10))]<-paste0('rep.',cn[(cn%in%as.character(1:10))])
  colnames(Analyte.Data)<-cn
  return(Analyte.Data)
}
