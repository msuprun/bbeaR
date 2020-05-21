Run_doNetCirclePlot <- function (ebfit, D, AnnotTable, fname, 
                                 circle.scale = 18.5) {
  for (comparison in colnames(ebfit$coef)) {
    if (ncol(D)==1){
      Dz<-cbind(D[order(rownames(D)),])
      colnames(Dz)[1]<-comparison
    } else {
      Dz<-D[order(rownames(D)),]
    }
    doNetCirclePlot_peptides(DContrast = Dz[, comparison], estimate = ebfit$coef[rownames(Dz), comparison], 
                             AnnotTable = AnnotTable[rownames(Dz),], 
                             fname = paste0(fname, "_NetFCHPlot_", comparison), main = paste0("lgFCH @ ", comparison), 
                             circle.scale = circle.scale, wt = 5, 
                             ht = 5)
  }
}
