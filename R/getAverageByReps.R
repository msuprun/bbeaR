getAveragesByReps <- function(eset, unit.reps.vars = c("SUBJECT.ID", "Visit", "Plate")) {
  eset$uf <- drop.levels(factor(apply(pData(eset)[, unit.reps.vars], 
                                      1, paste, collapse = "_")))
  mat_m <- sapply(levels(eset$uf), function(l, mat, uf) {
    if (sum(uf == l) == 1) {
      z <- mat[, which(uf == l)]
    }
    else {
      z <- rowMeans(mat[, which(uf == l)],na.rm=T)
    }
    return(z)
  }, exprs(eset), eset$uf, simplify = TRUE, USE.NAMES = TRUE)
  pd_m <- as.data.frame(t(sapply(colnames(mat_m), function(x) {
    unlist(strsplit(x, "_", fixed = T))
  })))
  colnames(pd_m) <- unit.reps.vars
  rownames(pd_m) <- colnames(mat_m)
  pd_noreps <- subset(pData(eset), !duplicated(uf))
  pd_out <- merge(mutate(pd_m, rownames1 = rownames(pd_m)), 
                  pd_noreps, unit.reps.vars, all.x = TRUE)
  rownames(pd_out) <- as.character(pd_out$rownames1)
  out <- ExpressionSet(assayData = as.matrix(mat_m))
  phenoData(out) <- new("AnnotatedDataFrame", as.data.frame(pd_out[colnames(mat_m), 
                                                                   ]))
  featureData(out) = featureData(eset)[featureNames(out), ]
  return(out)
}