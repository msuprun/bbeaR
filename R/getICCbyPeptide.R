getICCbyPeptide <- function(eset, UR = c("SUBJECT.ID", "Plate"), 
                            type = "a", model = "two") {
  require(irr)
  require(plyr)
  eset$unit.factor <- droplevels(factor(apply(pData(eset)[,UR], 1, paste, collapse = "_")))
  icc.a <- t(apply(exprs(eset), 1, function(x, UR) {
    db <- cbind.data.frame(x = x, UnitRep = UR)
    db <- ddply(db, .(UnitRep), transform, Rep = seq(x))
    dbh <- reshape(db, idvar = "UnitRep", timevar = "Rep", direction = "wide")
    dbh <- dbh[, (colSums(!is.na(dbh)) > nrow(dbh) * 0.4)]
    a <- icc(as.matrix(subset(dbh, select = -UnitRep)), type = type, model = model)
    out <- c(ICC = a$value, LCI = a$lbound, UCI = a$ubound)
    rm(a)
    return(out)
  }, UR = eset$unit.factor))
  icc.a <- cbind.data.frame(Peptide = rownames(icc.a), icc.a)
  return(icc.a)
}