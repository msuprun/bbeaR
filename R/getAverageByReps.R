getAveragesByReps <- function(eset, UR = c("PTID", "Plate")) {
  
  if(length(UR)>1) {
    eset$uf <- droplevels(factor(apply(pData(eset)[, UR], 1, paste, collapse = "_")))
  } else {
    eset$uf <- droplevels(factor(pData(eset)[, UR]))
  }
  
  mat_m <- sapply(levels(eset$uf), function(l, mat, uf) {
    if (sum(uf == l) == 1) {
      z <- mat[, which(uf == l)]
    }
    else {
      z <- rowMeans(mat[, which(uf == l)],na.rm=T)
    }
    return(z)
  }, exprs(eset), eset$uf, simplify = TRUE, USE.NAMES = TRUE)
  
  if(length(UR)>1) {
    pd_m <- as.data.frame(t(sapply(colnames(mat_m), function(x) {
      unlist(strsplit(x, "_", fixed = T))
    })))
    colnames(pd_m) <- UR
    rownames(pd_m) <- colnames(mat_m)
  } else {
    pd_m <- as.data.frame(colnames(mat_m))
    rownames(pd_m) <- pd_m[,1]
    colnames(pd_m) <- UR
  }
  
  pd_noreps <- subset(pData(eset), !duplicated(uf))
  pd_out <- merge(mutate(pd_m, rownames1 = rownames(pd_m)), pd_noreps, UR, all.x = TRUE)
  rownames(pd_out) <- as.character(pd_out$rownames1)
  pd_out$rownames1<-NULL
  out <- ExpressionSet(assayData = as.matrix(mat_m))
  phenoData(out) <- new("AnnotatedDataFrame", as.data.frame(pd_out[colnames(mat_m), ]))
  featureData(out) = featureData(eset)[featureNames(out), ]
  return(out)
}
