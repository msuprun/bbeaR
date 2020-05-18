BinarizeBinding<-function (object, buffer.name = "Buffer", sample.name = "Sample", 
                           conf.inf = 0.95, unit.reps.vars = c("SUBJECT.ID", "Visit")) {
  object$pData$unit.factor <- drop.levels(factor(apply(object$pData[, 
                                                                    unit.reps.vars], 1, paste, collapse = "_")))
  w_neg.ctrl <- rownames(subset(object$pData, (grepl(buffer.name, 
                                                     SampleType, fixed = T))))
  buffer.db <- melt(mutate(object$NetMFI[, w_neg.ctrl], Analyte = rownames(object$NetMFI)), 
                    id.vars = "Analyte")
  buffer.db <- merge(buffer.db, mutate(object$pData, variable = rownames(object$pData)), 
                     by = "variable", all.x = T)
  buffer.db$value <- log2(buffer.db$value + 0.5)
  alpha = 1 - (1 - conf.inf)/2
  buffer.db.ig <- ddply(buffer.db, .(PlateNum, Analyte), summarise, 
                        Buff.N = length(unique(Well_coord)), Buff.Mean = mean(value), 
                        Buff.SD = sd(value), Buff.SE = Buff.SD/sqrt(Buff.N), 
                        Buff.UCL = Buff.Mean + qt(alpha, df = Buff.N - 1) * Buff.SE, 
                        Buff.LCL = Buff.Mean - qt(alpha, df = Buff.N - 1) * Buff.SE)
  wb <- with(object$pData, grep(sample.name, SampleType, fixed = T))
  wb <- rownames(subset(object$pData, (grepl(sample.name, SampleType, 
                                             fixed = T))))
  pt.db <- melt(mutate(object$NetMFI[, wb], Analyte = rownames(object$NetMFI)), 
                id.vars = "Analyte")
  pt.db <- merge(pt.db, mutate(object$pData, variable = rownames(object$pData)), 
                 by = "variable", all.x = T)
  pt.db$value <- log2(pt.db$value + 0.5)
  pt.db.aux <- ddply(pt.db, .(PlateNum, Analyte, unit.factor), 
                     summarise, Pt.Mean = mean(value, na.rm = T))
  pt.db.ig <- merge(pt.db.aux, buffer.db.ig, by = c("PlateNum", 
                                                    "Analyte"), all.x = T)
  pt.db.ig <- merge(pt.db.ig, subset(pt.db, !duplicated(paste(PlateNum, 
                                                              unit.factor)), select = c("PlateNum", "unit.factor", 
                                                                                        unit.reps.vars)), by = c("PlateNum", "unit.factor"), 
                    all.x = T)
  pt.db.ig <- mutate(pt.db.ig, Ig.Binary = as.numeric(Pt.Mean > 
                                                        Buff.UCL))
  a <- pt.db.ig[, c("Analyte", "PlateNum", "unit.factor", "Ig.Binary", 
                    unit.reps.vars)]
  BinaryMat <- dcast(a, Analyte ~ PlateNum + unit.factor, value.var = "Ig.Binary")
  rownames(BinaryMat) <- BinaryMat[, 1]
  BinaryMat <- BinaryMat[, -1]
  a <- subset(mutate(pt.db.ig, A = paste(PlateNum, unit.factor, 
                                         sep = "_")), !duplicated(A), select = c("PlateNum", "unit.factor", 
                                                                                 unit.reps.vars, "A"))
  rownames(a) <- as.character(a$A)
  eset <- ExpressionSet(assayData = as.matrix(BinaryMat), phenoData = new("AnnotatedDataFrame", 
                                                                          subset(a[colnames(BinaryMat), ], select = -A)))
  return(eset)
}