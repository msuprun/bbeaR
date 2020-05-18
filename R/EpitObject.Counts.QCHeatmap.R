EpitObject.Counts.QCHeatmap<-function (object, filename = "HeatmapCounts.pdf", plateVar = c("PlateNum"), 
                                       ann = NULL, height = 10, width = 10) {
  require(ggplot2)
  require(reshape2)
  require(plyr)
  object$Count$Analyte <- rownames(object$Count)
  x <- melt(object$Count, id.vars = "Analyte")
  colnames(x) <- c("Analyte", "Sample", "CountSum")
  pd <- cbind.data.frame(Sample = rownames(object$pData), Plate = object$pData[, 
                                                                               plateVar])
  x <- merge(x, pd, by = "Sample", all.x = T)
  x <- x[order(x$Plate, x$Sample), ]
  x$Sample <- factor(as.character(x$Sample), levels = unique(as.character(x$Sample)))
  p <- ggplot(x, aes(y = Analyte, x = Sample)) + geom_tile(aes(fill = log2(CountSum + 
                                                                             1))) + labs(y = "") + scale_fill_gradient(low = "white", 
                                                                                                                       high = "steelblue")
  if (!is.null(ann)) {
    x <- merge(x, ann, by = "Analyte")
    p <- ggplot(x, aes(y = Peptide.Name, x = Sample)) + geom_tile(aes(fill = log2(CountSum + 
                                                                                    1))) + labs(y = "") + scale_fill_gradient(low = "white", 
                                                                                                                              high = "steelblue")
  }
  ggsave(file = filename, height = height, width = width, plot = p + 
           theme_bw() + theme(axis.text.x = element_text(angle = 90, 
                                                         hjust = 1)))
}