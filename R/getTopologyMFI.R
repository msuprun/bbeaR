getTopologynMFI<-function(eset,peptideDB,topoDB,endSignal=0) {
  #require(dplyr)
  require(plyr)
  require(stringr)
  require(reshape)
  
  etemp<-eset
  means_byPep<-cbind.data.frame(Mean=rowMeans(exprs(etemp),na.rm=T),Peptide=featureNames(etemp))
  means_byPep<-merge(means_byPep,peptideDB,by="Peptide",all.x=T)
  mer<-nchar(as.character(means_byPep$Sequence[1]))
  merLast<-nchar(as.character(means_byPep$Sequence[nrow(means_byPep)]))
  if(mer>merLast) {
    temp<-as.data.frame(matrix(unlist(strsplit(as.character(
      means_byPep$Sequence[1:nrow(means_byPep)-1]),"")),ncol=mer,byrow=TRUE))
    temp<-rbind.fill(temp,as.data.frame(t(unlist(strsplit(as.character(
      means_byPep$Sequence[nrow(means_byPep)]),"")))))
  } else {
    temp<-as.data.frame(matrix(unlist(strsplit(as.character(means_byPep$Sequence),"")),
                               ncol=mer,byrow=TRUE))
  }
  
  colnames(temp)<-paste0("S",1:mer)
  temp[]<-lapply(temp,as.character)
  means_byPep<-cbind(means_byPep,temp)  
  means_byPep<-melt(means_byPep[,c('Peptide','Mean',"Sequence","Peptide.Number",
                                   grep("^S[[:digit:]]",colnames(means_byPep),value=T))],
                    id.vars=c('Peptide','Mean',"Sequence",'Peptide.Number'))
  
  temp<-subset(means_byPep,variable=="S1")
  temp<-plyr::mutate(temp,diff=seq(0,((nrow(temp)-1)*2),2),start=Peptide.Number+diff,mmer=mer)
  
  means_byPep<-merge(means_byPep,temp[,c("Peptide",'diff','start',"mmer")],by="Peptide",all.x=T)
  means_byPep<-ddply(arrange(means_byPep,Peptide,variable),.(Peptide),transform,
                     AA.Number=seq(min(start),min(start)+(unique(mmer)-1)))
  
  means_byPep.avg<-ddply(means_byPep,.(value,AA.Number),here(summarise),
                         avg=mean(Mean,na.rm=T),med=median(Mean,na.rm=T),
                         max=max(Mean,na.rm=T),min=min(Mean,na.rm=T))
  means_byPep.avg<-subset(means_byPep.avg,!is.na(value))
  means_byPep.avgPlot<-merge(mutate(topoDB,AA=as.character(AA)),
                             mutate(means_byPep.avg,AA=value,value=NULL,AA.Number=AA.Number+endSignal),
                             by=c("AA.Number","AA"), all=T)
  
  temp<-subset(means_byPep[,c("Peptide","Sequence","AA.Number",'value','variable')],variable=="S1")
  temp<-mutate(temp,AA.Number=AA.Number+endSignal,AA=as.character(value))
  
  means_byPep.avgPlot<-merge(means_byPep.avgPlot,temp[,c("Peptide","Sequence","AA.Number",'AA')],
                             by=c("AA.Number","AA"),all.x=T)
  means_byPep.avgPlot<-mutate(means_byPep.avgPlot,PepNum=gsub("(^|[^0-9])0+","\\1",
                                                              str_extract(Peptide,"[[:digit:]]{1,3}")))
  return(means_byPep.avgPlot)
}
