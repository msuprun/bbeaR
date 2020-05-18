
getLongBinding <- function(int.mat, pd, make.plot=FALSE,fname='') {
  binding.long.db <- plyr::rename(melt((int.mat)), c(X1 = "Peptide", 
                                                     X2 = "Sample", value = "Binding"))
  binding.long.db$Sample<-as.character(binding.long.db$Sample)
  db.temp <- cbind.data.frame(Sample=as.character(colnames(int.mat)), pd)
  binding.long.db <- merge(binding.long.db, db.temp, by='Sample')
  binding.long.db <- mutate(binding.long.db, 
                            Plate=factor(Plate),
                            LETTER = factor(substr(Well_coord, 1, 1), levels = LETTERS[12:1]), 
                            Number = factor(mgsub(LETTERS[1:12], rep("", 12), Well_coord), levels = 1:12))
  nb <- length(levels(binding.long.db$Plate))
  if (make.plot) {
    p <- ggplot(binding.long.db, aes(x = reorder(Well_coord, print.plate.order), y = Binding)) + 
      geom_point(size = 1, aes(colour = LETTER)) + 
      geom_smooth(aes(x = print.plate.order, y = Binding), colour = "red") + 
      theme(axis.text.x = element_text(angle=90,vjust=0.5, hjust=1,size=9)) + 
      facet_wrap(~Plate, ncol = 1)
    ggsave(file=paste0(fname, "BindingByWell.pdf"), height=3.3 *nb, width=14, plot=p)
  }
  
  return(binding.long.db)
}