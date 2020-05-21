makeTopologyPlotBase <- function(db) {
  require(ggplot2)
  
  p <- ggplot(db,aes(y=ylvl,x=xlvl)) +
    theme(panel.background=element_blank(), 
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank()) 
  
  return(p)
}