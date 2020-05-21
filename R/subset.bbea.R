subset.bbea <- function(bbea.obj, statement) {
  z<-rownames(bbea.obj$pData)[statement==TRUE]
  print(paste0(length(z)," Samples meet the statement criteria"))
  L1<-lapply(bbea.obj[(names(bbea.obj)%in%c("Median","NetMFI","Count"))], function(l){l[,z]})
  L2<-lapply(bbea.obj[(names(bbea.obj)%in%c("pData"))], function(l){l[z,]})
  if (class(bbea.obj$pData$Plate)=="factor") {
    L2$pData$Plate <- droplevels(L2$pData$Plate)
  }
  return(c(L1,L2))
}
