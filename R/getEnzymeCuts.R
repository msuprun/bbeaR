getEnzymeCuts <- function(sequence,topo = FALSE, topodb = NULL, rmsignal=T, signalend=24) {
  require(cleaver)
  
  a1 <- cbind.data.frame(AA.Number=cleavageSites(sequence, enzym=c("pepsin"))[[1]],Pepsin='P')
  a2 <- cbind.data.frame(AA.Number=cleavageSites(sequence, enzym=c("trypsin"))[[1]],Trypsin='T')
  a3 <- cbind.data.frame(AA.Number=cleavageSites(sequence, enzym=c("chymotrypsin-high"))[[1]],Chymotrypsin='Ch')
  
  enzymes <- merge(a1,a2,by="AA.Number",all=T)
  enzymes <- merge(enzymes,a3,by="AA.Number",all=T)
  enzymes[,-1] <- sapply(enzymes[,-1], as.character) 
  enzymes[is.na(enzymes)] <- ""
  enzymes <- plyr::mutate(enzymes,Enzyme=paste(Trypsin,Chymotrypsin,Pepsin))
  
  if (rmsignal) {
    enzymes <- plyr::mutate(enzymes,EnzymeRmSignal=ifelse(AA.Number<(signalend+1),"",Enzyme))
  }
  
  if (topo) {
    enzymes <- merge(topodb,enzymes,by='AA.Number',all.x=T)
  } 
  
  return(enzymes)
  
}
