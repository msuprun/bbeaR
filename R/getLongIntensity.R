
getLongIntensity <- function(int.mat, pd, laby = NULL, fname = NULL, make.plot=TRUE) {
  Intensity.long.db <- plyr::rename(melt((int.mat)), c(X1 = "Peptide", 
                                                       X2 = "Sample", value = "Intensity"))
  Intensity.long.db$Sample<-as.character(Intensity.long.db$Sample)
  db.temp <- cbind.data.frame(Sample=as.character(colnames(int.mat)), pd)
  Intensity.long.db <- merge(Intensity.long.db, db.temp, by='Sample')
  buffer <- unique(as.character(pd$SUBJECT.ID[grep("Buffer", pd$SUBJECT.ID)]))
  buffer.ur <- unique(as.character(pd$UnitRep[grep("Buffer", pd$UnitRep)]))
  Intensity.long.db <- mutate(Intensity.long.db, 
                              SUBJECT.ID = relevel(factor(mapvalues(SUBJECT.ID, buffer, "buffer")), ref = "buffer"), 
                              UnitRep = relevel(factor(mapvalues(UnitRep, buffer.ur, "buffer")), ref = "buffer"), 
                              Plate = factor(Plate), 
                              LETTER = factor(substr(Well_coord, 1, 1), levels = LETTERS[12:1]), 
                              Number = factor(mgsub(LETTERS[1:12], rep("", 12), Well_coord), 
                                              levels = 1:12))
  nb <- length(levels(Intensity.long.db$Plate))
  if (make.plot) {
    p <- ggplot(Intensity.long.db, aes(x = reorder(Well_coord, print.plate.order), y = Intensity)) + 
      geom_point(size = 1, aes(colour = LETTER)) + 
      geom_smooth(aes(x = print.plate.order, y = Intensity), colour = "red") + 
      theme(axis.text.x = element_text(angle=90,vjust=0.5, hjust=1,size=9)) + 
      facet_wrap(~Plate, ncol = 1) + 
      labs(y = laby)
    ggsave(file = paste0(fname, ".pdf"), height = 3.3 * nb, width = 14, plot = p)
  }
  return(Intensity.long.db)
}