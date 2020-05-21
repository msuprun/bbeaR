MFI2nMFI <- function(bbea.obj, offset=0.5, rmNeg=TRUE){
  # old name PreprocessEpitCompound()
  sample_neg_ctrl<-c()
  nMFIEpit.preproc<-bbea.obj
  if (class(bbea.obj$pData$Plate)!="factor") { 
    bbea.obj$pData$Plate <- factor(bbea.obj$pData$Plate) 
  }
  for (b in levels(bbea.obj$pData$Plate)){
    print(b)
    wb <- rownames(subset(bbea.obj$pData, Plate==b)) 
    w_neg.ctrl<-rownames(subset(bbea.obj$pData, (Plate==b)&(grepl("Buff",Sample,fixed=T))))
    
    NSB_lg       <- rowMeans(log2(bbea.obj$Median[,w_neg.ctrl]+offset),na.rm=T)
    Median_lg    <- log2(bbea.obj$Median[,wb] + offset)
    Median_norm  <- (Median_lg - NSB_lg) 
    
    if (rmNeg) {
      Median_norm <- t(apply(Median_norm, 1, function(x,y){ x[(x<0)]<-0; return(x) }))
    } 
    
    bbea.obj$Median[,wb]<-Median_norm
    sample_neg_ctrl<-c(w_neg.ctrl,sample_neg_ctrl)
  }
  samples.in<-setdiff(colnames(bbea.obj[["Median"]]), sample_neg_ctrl)
  nMFIEpit.preproc[["Median"]]<-bbea.obj[["Median"]][,samples.in]
  nMFIEpit.preproc[["pData"]]<- bbea.obj[["pData"]][samples.in,]
  nMFIEpit.preproc[["AssayInfo"]]<- bbea.obj[["AssayInfo"]][samples.in,]
  names(nMFIEpit.preproc)<-gsub('Median','nMFI',names(nMFIEpit.preproc))
  return(nMFIEpit.preproc)
}