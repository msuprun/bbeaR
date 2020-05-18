
getICCByReps.Analyte<-function (eset, unit.reps.vars = c("SUBJECT.ID", "Visit", "Plate"), 
                                type = "a", model = "two") {
  require(irr)
  eset$unit.factor <- drop.levels(factor(apply(pData(eset)[,unit.reps.vars], 1, paste, collapse = "_")))
  #aux <- function(x) {c(1:length(x))}
  icc.a <- t(apply(exprs(eset), 1, function(x, UR) {
    db <- cbind.data.frame(x = x, UnitRep = UR)
    db <- ddply(db, .(UnitRep), mutate, Rep = seq(x))
    dbh <- reshape(db, idvar = "UnitRep", timevar = "Rep", 
                   direction = "wide")
    dbh <- dbh[, (colSums(!is.na(dbh)) > nrow(dbh) * 0.4)]
    a <- icc(as.matrix(subset(dbh, select = -UnitRep)), type = type, 
             model = model)
    out <- c(value = a$value, LCI = a$lbound, UCI = a$ubound)
    rm(a)
    return(out)
  }, UR = eset$unit.factor))
  icc.a <- cbind.data.frame(Peptide = rownames(icc.a), icc.a)
  return(icc.a)
}