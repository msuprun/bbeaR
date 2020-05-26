bbea.QC.heatmap.counts.byPlate <- function(bbea.obj, getlog2=FALSE,
                                           filename='HeatmapCounts', plateVar="PlateNum",
                                           ann=NULL, he=7, wi=9) {
  # aka EpitObject.Counts.QCHeatmap.byPlate
  require(ggplot2)
  require(reshape2)
  require(plyr)
  
  for (i in as.character(unique(bbea.obj$pData[,plateVar]))) {
    print(i)
    objectsub <- bbea.subset(bbea.obj,(bbea.obj$pData[,plateVar]==i))
    x <- reshape2::melt(mutate(objectsub$Count,Analyte=rownames(objectsub$Count)),id.var="Analyte")
    colnames(x) <- c('Analyte','Sample','Count')
    pd <- cbind.data.frame(Sample=rownames(objectsub$pData),Plate=objectsub$pData[,plateVar])
    pd <- mutate(pd,SamplePos=factor(str_extract(
      sapply(strsplit(as.character(Sample),"\\.1\\."),'[',2),"[A-H][[:digit:]]{1,2}"),
      levels=paste0(rep(LETTERS[1:8],each=12),1:12)))
    
    if (getlog2) {
      x$Count <- log2(x$Count+1)
    }
    
    x <- merge(x,pd,by='Sample',all.x=T)
    x<-x[order(x$Plate,x$SamplePos),]
    
    if (!is.null(ann)) {
      x <- merge(x,ann,by="Analyte")
      p<-ggplot(x, aes(y=Peptide, x=SamplePos)) +
        geom_tile(aes(fill = Count)) + labs(y="",x="") + theme_bw() +
        theme(axis.text.x=element_text(size=6,angle=90,hjust=1,vjust=0.5)) +
        scale_fill_gradient("log2(Count+1)",low = "white",high = "steelblue")
    } else {
      p<-ggplot(x, aes(y=Analyte, x=SamplePos)) +
        geom_tile(aes(fill = Count)) + labs(y="",x="") + theme_bw() +
        theme(axis.text.x=element_text(size=6,angle=90,hjust=1,vjust=0.5)) +
        scale_fill_gradient("Count",low = "white",high = "steelblue")
    }
    ggsave(file=paste0(filename,".",i,".pdf"), height=he, width=wi, plot=p)
  }
  
}
