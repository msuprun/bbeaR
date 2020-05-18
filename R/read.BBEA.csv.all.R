read.BBEA.csv.all <- function(filenames) {
  require(stringr)
  Experiment.Plates <- lapply(filenames, read.BBEA.csv)
  Median <- Reduce(bbea.merge, lapply(lapply(Experiment.Plates, function(l) {l$Median}), 
                                      function(x) data.frame(x, rn = row.names(x))))
  colnames(Median) <- str_replace_all(colnames(Median), "[[:punct:]]", ".")
  rownames(Median) <- Median$rn
  Median$rn<-NULL
  
  NetMFI <- Reduce(bbea.merge, lapply(lapply(Experiment.Plates, function(l) {l$NetMFI}), 
                                      function(x) data.frame(x, rn = row.names(x))))
  colnames(NetMFI) <- str_replace_all(colnames(NetMFI), "[[:punct:]]", ".")
  rownames(NetMFI) <- NetMFI$rn
  NetMFI$rn <- NULL
  
  Count <- Reduce(bbea.merge, lapply(lapply(Experiment.Plates, function(l) {l$Count}), 
                                     function(x) data.frame(x, rn = row.names(x))))
  colnames(Count) <- str_replace_all(colnames(Count), "[[:punct:]]", ".")
  rownames(Count) <- Count$rn
  Count$rn<-NULL
  
  pData <- do.call("rbind", lapply(Experiment.Plates, function(l) {l$pData}))
  rownames(pData) <- str_replace_all(rownames(pData), "[[:punct:]]", ".")
  AssayInfo <- do.call("rbind", lapply(Experiment.Plates, function(l) {l$AssayInfo}))
  bbea.Plates <- list(Median = Median, NetMFI = NetMFI, Count = Count, 
                      pData = pData, AssayInfo = AssayInfo)
  return(bbea.Plates)
}
