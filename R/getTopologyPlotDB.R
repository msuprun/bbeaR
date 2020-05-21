getTopologyPlotDB <- function(aaseq,ymax=21,offset=4,meta=FALSE,metadb=NULL){
  require(plyr)
  
  aaseq <- as.character(aaseq)
  plength <- nchar(aaseq)
  xfolds <- ceiling((plength-offset)/ymax)
  
  db <- cbind.data.frame(AA=unlist(strsplit(aaseq, '')),
                         AA.Number=seq(1:nchar(aaseq)))
  db <- plyr::mutate(db,ylvl="",xlvl="")
  
  db$ylvl[1:offset] <- seq(-offset,-1)
  db$ylvl[(offset+1):plength] <- c(seq(0,ymax),seq(ymax,0))
  db$xlvl[1:(offset+1)] <- 0
  
  z<-c()
  for (i in 1:xfolds) {
    z <- c(z,c(rep(i-1,(ymax-1)),((i-1)+0.27),((i-1)+0.73)))
  }
  
  db$xlvl[(offset+2):plength]<-z[1:(plength-offset)]
  
  if (meta) {
    signal <- seq(metadb$begin[grep("SIGNAL",metadb$type)],metadb$end[grep("SIGNAL",metadb$type)])  
    carbs <- ifelse(grepl("CARBOHYD",metadb$type),metadb$begin,NA)
    disulfb <- metadb$begin[grep("DISULFID",metadb$type)]   
    disulfe <- metadb$end[grep("DISULFID",metadb$type)]   
    variantb <- metadb$begin[grep("VARIANT",metadb$type)]  
    variante <- metadb$end[grep("VARIANT",metadb$type)]  
    
    db <- plyr::mutate(db, type=ifelse(AA.Number%in%signal,"Signal",''),
                       variant=ifelse(AA.Number%in%c(variantb,variante),"Yes",''),
                       glycosylation=ifelse(AA.Number%in%carbs,"Yes",''),
                       disulf=ifelse(AA.Number%in%c(disulfb,disulfe),"SS",''))
  }
  
  db <- plyr::mutate(db,ylvl=as.numeric(as.character(ylvl)),
                     xlvl=as.numeric(as.character(xlvl)))
  
  return(db)
}
