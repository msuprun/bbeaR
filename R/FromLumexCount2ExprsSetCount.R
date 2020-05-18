
FromLuminexCount2ExprsSetCount<-function(epit.object, Annot=NULL){
  if (!is.null(Annot)){
    analyte.db<-merge(data.frame(Analyte=rownames(epit.object$Count)),
                      Annot,by='Analyte',by.x=TRUE)
  } else {
    analyte.db<-cbind.data.frame(Analyte=rownames(epit.object$Count))
  }
  rownames(analyte.db)<-as.character(analyte.db$Analyte)
  pd<-as.data.frame(epit.object$pData)
  analyte.db<-as.data.frame(analyte.db[rownames(epit.object$Count),])
  eset<-ExpressionSet(assayData=as.matrix(epit.object$Count),
                      featureData=new("AnnotatedDataFrame",analyte.db),
                      phenoData=new("AnnotatedDataFrame",pd))
  return(eset)
}