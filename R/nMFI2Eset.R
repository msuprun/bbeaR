nMFI2Eset <- function(nMFI.object){
  require(Biobase)
  # old name FromLuminexBinding2ExprsSet()
  pd<-as.data.frame(nMFI.object$pData)
  eset<-ExpressionSet(assayData=as.matrix(nMFI.object$nMFI))
  pData(eset)<-pd[sampleNames(eset),]
  return(eset)
}