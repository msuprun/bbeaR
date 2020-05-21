getCVbyPeptide <- function(eset,UR="PTID"){
  require(plyr)
  eset$IDr<-apply(pData(eset)[,UR], 1, paste, collapse = "_")
  uID<-unique(pData(eset)[,"IDr"])
  db<-sapply(uID,function(l,mat){
    print(l)
    uIDp<-sampleNames(eset)[which(pData(eset)[,"IDr"]==l)]
    if (length(uIDp)>1) {
      # adding constant 1 to all values to avoid having a mean of 0,
      # which is a denominator of CV
      db<-data.frame(mean=apply(abs(mat[,uIDp]+1),1,mean,na.rm=T), 
                     sd=apply(abs(mat[,uIDp]+1),1,sd,na.rm=T),
                     Peptide=rownames(mat))
      db<-mutate(db,cv=sd/mean*100)
    } else {
      db<-as.data.frame(cbind(mean=NA,sd=NA,Peptide=NA,cv=NA))
    }
    return(db)
  },mat=exprs(eset),simplify=F)
  db2<-ddply(ldply(db,"data.frame"),.(Peptide),here(summarise),
             mean.cv=mean(cv,na.rm=T),sd.cv=sd(cv,na.rm=T),
             se.cv=sd.cv/sqrt(length(db)),
             median.cv=median(cv,na.rm=T))
  db2<-subset(db2,!is.na(Peptide))
  return(db2)
}
