
EpitObject.QCSamples<-function (object, filename = "./QC.Samples.", plateVar = "PlateNum", 
                                shapeVar = NULL, sizeVar = NULL, height = 4, width = 5.5) {
  require(ggplot2)
  obj <- object$pData
  if (is.null(shapeVar)) 
    theshapes <- rep("10000", nrow(obj))
  else theshapes <- obj[, shapeVar]
  if (is.null(sizeVar)) 
    thesizes <- factor(!grepl("IPC|Buff", theshapes), labels = c("Control", 
                                                                 "Patient"))
  else thesizes <- obj[, sizeVar]
  obj <- cbind(obj, PlateN = as.factor(obj[, plateVar]), shapeVar = factor(theshapes), 
               sizeVar = factor(thesizes))
  mytheme <- theme_bw() + theme(axis.text.x = element_text(angle = 90, 
                                                           hjust = 1))
  p1 <- ggplot(obj, aes(PlateN, y = log2(CountSum), color = PlateN)) + 
    geom_point(aes(shape = shapeVar, size = sizeVar)) + scale_size_manual(values = c(3, 
                                                                                     1)) + labs(x = "", title = "Sum of Counts for all Samples") + 
    guides(colour = FALSE) + mytheme
  p2 <- ggplot(subset(obj, CountSum > 30), aes(PlateN, y = log2(CountSum))) + 
    geom_point(aes(shape = shapeVar, size = sizeVar, color = PlateN)) + 
    scale_size_manual(values = c(3, 1)) + labs(x = "Plate", 
                                               title = "Sum of Counts (>30)") + guides(colour = FALSE) + 
    mytheme
  p3 <- ggplot(obj, aes(PlateN, y = CountMin, color = PlateN)) + 
    geom_point(aes(shape = shapeVar, size = sizeVar)) + scale_size_manual(values = c(3, 
                                                                                     1)) + labs(x = "Plate", title = "Min Counts for all Samples") + 
    guides(colour = FALSE) + mytheme
  ggsave(file = paste0(filename, "CountSum.", Sys.Date(), ".pdf"), 
         height = height, width = width, plot = p1)
  ggsave(file = paste0(filename, "CountSumGT30.", Sys.Date(), 
                       ".pdf"), height = height, width = width, plot = p2)
  ggsave(file = paste0(filename, "CountMin.", Sys.Date(), ".pdf"), 
         height = height, width = width, plot = p3)
}