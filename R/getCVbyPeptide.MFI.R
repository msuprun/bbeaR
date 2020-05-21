getCVbyPeptide.MFI<-function(bbea.obj=bbeaE,UR=c("PTID","Plate")){
  require(plyr)
  mat<-bbea.obj$Median
  pd<-bbea.obj$pData
  pd$IDr<-apply(pd[,UR], 1, paste, collapse = "_")
  uID<-unique(pd[,"IDr"])
  db<-sapply(uID,function(l,mat){
    print(l)
    uIDp<-rownames(pd)[which(pd[,"IDr"]==l)]
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
  },mat=mat,simplify=F)
  #https://stackoverflow.com/questions/6955128/object-not-found-error-with-ddply-inside-a-function
  db2<-ddply(ldply(db,"data.frame"),.(Peptide),here(summarise),
             mean.cv=mean(cv,na.rm=T),sd.cv=sd(cv,na.rm=T),
             se.cv=sd.cv/sqrt(length(db)),
             median.cv=median(cv,na.rm=T))
  db2<-subset(db2,!is.na(Peptide))
  return(db2)
}
