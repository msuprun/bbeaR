bbea.changeAnnotation <- function(bbea.obj, annotation, newNameCol="Peptide"){
  # annot needs to have a Bead column with a numeric bead #
  rownames(annotation) <- paste0("Analyte ", as.character(annotation$Bead))
  annotation$Peptide <- as.character(annotation[,newNameCol])
  rownames(bbea.obj$Median) <- annotation[rownames(bbea.obj$Median), "Peptide"]
  rownames(bbea.obj$NetMFI) <- annotation[rownames(bbea.obj$NetMFI), "Peptide"]
  rownames(bbea.obj$Count) <- annotation[rownames(bbea.obj$Count), "Peptide"]
  return(bbea.obj)
}
