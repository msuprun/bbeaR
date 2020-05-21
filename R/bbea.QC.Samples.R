bbea.QC.Samples <- function(bbea.obj, filename="QC.Samples.", plateVar='Plate',
                           gt=30, jitterw=0.2, height=4, width=5.5) {
  
  require(ggplot2)
  obj<-bbea.obj$pData
  obj <- cbind(obj, PlateN=as.factor(obj[,plateVar]))
  mytheme<-theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  p1 <- ggplot(obj, aes(PlateN, y=CountMean, color=PlateN)) +
    geom_jitter(width=jitterw) +
    scale_size_manual(values=c(3,1)) +
    labs(x="", title="Average of Counts for all Samples") +
    guides(colour=FALSE) +  mytheme
  
  p2 <- ggplot(subset(obj,CountMean>gt), aes(PlateN, y=CountMean, color=PlateN)) +
    geom_jitter(width=jitterw) +
    scale_size_manual(values=c(3,1)) +
    labs(x="", title=paste0("Average Counts >",gt)) +
    guides(colour=FALSE) +  mytheme
  
  p3 <- ggplot(obj, aes(PlateN, y=CountMin, color=PlateN)) +
    geom_jitter(width=jitterw) +
    scale_size_manual(values=c(3,1)) +
    labs(x="", title="Min Counts for all Samples") +
    guides(colour=FALSE) +  mytheme
  
  ggsave(file=paste0(filename,"CountMean.pdf"), height=height, width=width, plot=p1)
  ggsave(file=paste0(filename,"CountMeanGT",gt,".pdf"), height=height, width=width, plot=p2)
  ggsave(file=paste0(filename,"CountMin.pdf"), height=height, width=width, plot=p3)
  return(list(pavg=p1,pmin=p3))
}
