bbea.QC.heatmap.counts <- function(bbea.obj, getlog2=FALSE,
                                   filename='HeatmapCounts.pdf', plateVar="Plate",
                                   ann=NULL, height=9, width=10) {
  require(ggplot2)
  require(reshape2)
  require(plyr)
  
  x <- reshape2::melt(mutate(as.data.frame(bbea.obj$Count),
                             Analyte=rownames(bbea.obj$Count)),id.var="Analyte")
  colnames(x) <- c('Analyte','Sample','Count')
  pd <- cbind.data.frame(Sample=rownames(bbea.obj$pData),Plate=bbea.obj$pData[,plateVar])
  
  if (getlog2) {
    x$Count <- log2(x$Count+1)
  }
  
  x <- merge(x,pd,by='Sample',all.x=T)
  x<-x[order(x$Plate,x$Sample),]
  x$Sample<-factor(as.character(x$Sample),levels=unique(as.character(x$Sample)))
  
  p<-ggplot(x, aes(y=Analyte, x=Sample)) +
    geom_tile(aes(fill = Count)) + labs(y="") +
    scale_fill_gradient("Count",low = "white",high = "steelblue")
  ggsave(file=filename, height=height, width=width,
         plot=p+theme_bw()+theme(axis.text.x = element_text(angle = 90,size=3)))
  
  if (!is.null(ann)) {
    x <- merge(x,ann,by="Analyte")
    p<-ggplot(x, aes(y=Peptide, x=Sample)) +
      geom_tile(aes(fill = Count)) + labs(y="") +
      scale_fill_gradient("log2(Count+1)",low = "white",high = "steelblue")
    ggsave(file=gsub(".pdf",".Peptide.pdf",filename,fixed=T), height=height, width=width,
           plot=p+theme_bw()+theme(axis.text.x = element_text(angle = 90,size=3)))
  }
  return(p)
}