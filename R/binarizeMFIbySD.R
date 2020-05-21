binarizeMFIbySD <- function(bbea.object,buffer.name="Buffer",plateVar="PlateNum",
                            UR=c('PTID','Visit'),nSD=3,logMFI=TRUE,offset=0.5){
  require(reshape2)
  require(plyr)
  
  bbea.object$pData$plateVar<-bbea.object$pData[,plateVar]
  
  if(length(UR)>1) {
    bbea.object$pData$unit.factor<-droplevels(factor(apply(bbea.object$pData[, UR], 1, paste, collapse = "_")))
  } else {
    bbea.object$pData$unit.factor <- droplevels(factor(bbea.object$pData[, UR]))
  }
  
  if (logMFI) { bbea.object$Median<-log2(bbea.object$Median+offset) } 
  
  w_neg.ctrl<-rownames(subset(bbea.object$pData,(grepl(buffer.name,unit.factor,fixed=T))))
  buffer.db<-reshape2::melt(plyr::mutate(as.data.frame(bbea.object$Median[,w_neg.ctrl]),
                                         Peptide=rownames(bbea.object$Median)),id.vars="Peptide")
  buffer.db<-merge(buffer.db,mutate(bbea.object$pData,variable=rownames(bbea.object$pData)),
                   by="variable",all.x=T)
  
  buffer.db.ig<-plyr::ddply(buffer.db,.(plateVar,Peptide),here(summarise),
                            Buff.Mean=mean(value,na.rm=T),
                            Buff.SD=sd(value,na.rm=T),
                            Buff.uSD=Buff.Mean+Buff.SD*nSD) 
  
  wb<-rownames(subset(bbea.object$pData,(!grepl(buffer.name,unit.factor,fixed=T)))) 
  pt.db<-melt(plyr::mutate(as.data.frame(bbea.object$Median[,wb]),
                           Peptide=rownames(bbea.object$Median)),id.vars="Peptide")
  pt.db<-merge(pt.db,plyr::mutate(bbea.object$pData, variable=rownames(bbea.object$pData)),by="variable",all.x=T)
  
  pt.db.aux<-plyr::ddply(pt.db,.(plateVar,Peptide,unit.factor), here(summarise), Pt.Mean=mean(value,na.rm = T))
  pt.db.ig<-merge(pt.db.aux, buffer.db.ig, by=c('plateVar',"Peptide"),all.x=T)
  pt.db.ig<-merge(pt.db.ig, subset(pt.db, !duplicated(paste(plateVar,unit.factor)),
                                   select=c('plateVar','unit.factor',UR)), 
                  by=c('plateVar',"unit.factor"),all.x=T)
  pt.db.ig<-plyr::mutate(pt.db.ig, Ig.Binary=as.numeric(Pt.Mean>Buff.uSD))
  a<-pt.db.ig[,c('Peptide','plateVar','unit.factor','Ig.Binary',UR)]
  BinaryMat<-reshape2::dcast(a,Peptide~plateVar+unit.factor,value.var='Ig.Binary')
  rownames(BinaryMat)<-BinaryMat[,1]
  BinaryMat<-BinaryMat[,-1]
  a<-subset(mutate(pt.db.ig, A=paste(plateVar,unit.factor,sep='_')), !duplicated(A),
            select=c('plateVar', 'unit.factor', UR, 'A'))
  rownames(a)<-as.character(a$A)
  eset<-ExpressionSet(assayData = as.matrix(BinaryMat), 
                      phenoData=new("AnnotatedDataFrame",subset(a[colnames(BinaryMat),],select=-A)))
  return(eset)
}
