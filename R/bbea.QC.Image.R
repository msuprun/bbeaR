bbea.QC.Image <- function(db,ht=4,wd=6,filename,txtsize=2,hjust=0.5,
                          plate.design.db=NULL) {
  require(ggplot2)
  require(stringr)
  require(plyr)
  
  if (is.null(plate.design.db)){
    plate.design.db<-as.data.frame(cbind(Well_coord=paste0(rep(LETTERS[1:8],each=12),1:12),
                           print.plate.order=1:96))
  }
  
  image.db.plate <- merge(db, plate.design.db, by='Well_coord',all.x=TRUE,all.y=TRUE)
  image.db.plate <- plyr::mutate(image.db.plate,
                                 Well.Letter=factor(str_extract(as.character(Well_coord),"[A-Z]"),
                                                    levels=LETTERS[8:1]),
                                 Well.Number=factor(str_extract(as.character(Well_coord),"[[:digit:]]{1,2}"),
                                                    levels=1:12))
  labledb<-arrange(image.db.plate[,c("Well.Letter","Well.Number","Well_coord","Sample")],Well_coord)
  labledb<-labledb[!duplicated(labledb$Sample),]
  
  ttl <- sapply(strsplit(as.character(na.omit(unique(image.db.plate$filename))[[1]]),"_"),tail,2)[1]
  
  p <- ggplot(image.db.plate,aes(y=Well.Letter,x=Well.Number)) + theme_bw() +
    geom_tile(aes(fill=Sample),color="black",show.legend = F) +
    geom_text(data=labledb,aes(label=Sample),hjust=hjust,size=txtsize) +
    labs(x="",y="",title=paste0("Experimental Date: ",ttl))
  ggsave(file=paste0(filename,ttl,".pdf"),plot=p,height=ht,width=wd)
  return(p)
}
