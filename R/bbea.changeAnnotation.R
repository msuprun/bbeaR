bbea.changeAnnotation<-function(bbea.obj, annotation, newNameCol="Peptide",AnalyteCol="Analyte"){
  rownames(annotation) <- as.character(annotation[,AnalyteCol])
  annotation$Peptide <- as.character(annotation[,newNameCol])
  rownames(bbea.obj$Median) <- annotation[rownames(bbea.obj$Median), "Peptide"]
  rownames(bbea.obj$NetMFI) <- annotation[rownames(bbea.obj$NetMFI), "Peptide"]
  rownames(bbea.obj$Count) <- annotation[rownames(bbea.obj$Count), "Peptide"]
  return(bbea.obj)
}
