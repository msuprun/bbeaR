
FromLuminexBinding2ExprsSet <- function(Binding.object=binding.Milk, Annot = NULL, 
                                        feature.name = "Analyte", ordered.as.in.Annot = F) {
  
  pd <- as.data.frame(Binding.object$pData)
  
  eset <- ExpressionSet(assayData = as.matrix(Binding.object$Binding), 
                        phenoData = new("AnnotatedDataFrame", pd[, !duplicated(colnames(pd))]))
  
  if (!is.null(Annot)) {
    analyte.db <- plyr::rename(data.frame(x = rownames(Binding.object$Binding)), c(x = feature.name))
    analyte.db <- merge(analyte.db, Annot, by = feature.name, by.x = TRUE)
    rownames(analyte.db) <- as.character(analyte.db[, feature.name])
    analyte.db <- as.data.frame(analyte.db[rownames(Binding.object$Binding), ])
    eset <- ExpressionSet(assayData = as.matrix(Binding.object$Binding), 
                          featureData = new("AnnotatedDataFrame", analyte.db), 
                          phenoData = new("AnnotatedDataFrame", pd[, !duplicated(colnames(pd))]))
    
  }
  
  if (ordered.as.in.Annot==TRUE) {
    ordered.as.in.Annot <- (as.character(Annot[, feature.name]))
    eset <- eset[ordered.as.in.Annot, ]
  }
  return(eset)
}