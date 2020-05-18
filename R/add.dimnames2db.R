add.dimnames2db <- function(x, cn=colnames(x), rn=rownames(x)) {
  x <- as.data.frame(x)
  colnames(x) <- cn
  rownames(x) <- rn
  return(x)
}