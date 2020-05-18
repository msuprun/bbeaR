getPCAs<-function (eset, maxpc = 3) {
  if (!is.matrix(eset)) {
    ex <- exprs(eset)
    out <- pData(eset)
  }
  else {
    out <- NULL
    ex <- eset
  }
  pca.res <- prcomp(t(ex))
  x <- pca.res$x[, 1:maxpc]
  colnames(x) <- paste("PC", 1:maxpc, sep = ".")
  varex = round(100 * summary(pca.res)$importance[2, ])
  db <- cbind.data.frame(x)
  if (!is.null(out)) 
    db <- cbind(db, out)
  return(list(db = as.data.frame(db), varex = varex))
}