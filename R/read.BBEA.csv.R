read.BBEA.csv<-function (fname) {
  require(stringr)
  require(plyr)
  
  fname.red <- tail(unlist(strsplit(fname,'/')),1)
  print(paste0("Reading file: ",fname.red))
  
  temp <- readLines(fname, ok = TRUE, warn = FALSE, encoding = "unknown", skipNul = FALSE)
  ncols <- length(unlist(strsplit(temp[grep("Median",temp) + 1], ",")))
  
  X <- read.csv(fname, stringsAsFactors = FALSE, header = FALSE, col.names = paste0("V", 1:ncols))
  
  L <- vector('list',5)
  names(L) <- c('Median','NetMFI','Count','AssayInfo','pData')
  
  start_assay_info <- grep('Program',X[,1]);  end_assay_info<-grep('Samples',X[,1])
  dat_assay <- X[start_assay_info:end_assay_info, 1:14]
  dat_assay <- rbind(dat_assay,c("filename", fname.red, rep("",ncol(dat_assay)-2)))
  plate <- gsub(" ","", dat_assay[which(grepl('Batch',dat_assay[,1]))[1],2],fixed=T)
  plate_date <- as.Date(dat_assay[which(grepl('Date',dat_assay[,1]))[1],2],format="%m/%d/%y")
  plate_time <- dat_assay[which(grepl('Date',dat_assay[,1]))[1],3]
  plate_timeD <- sapply(strsplit(plate_time," ",fixed=T),"[",2)
  plate_timeHRD <- as.numeric(sapply(strsplit(plate_time,":",fixed=T),"[",1))
  plate_timeHR <- ifelse(plate_timeD=="AM"|plate_timeHRD==12,plate_timeHRD,plate_timeHRD+12)
  plate_timeHR <- ifelse(plate_timeD=="AM"&plate_timeHRD==12,0,plate_timeHR)
  
  index_data <- grep('DataType:', X[,1])
  index_data <- index_data[X[index_data,2]%in%c('Median','Net MFI','Count','Avg Net MFI')]
  
  for (i in 1:3){
    cnames <- X[(index_data[i]+1),]
    dat_db <- X[(index_data[i]+2):(index_data[(i+1)]-1),]
    dat_db <- dat_db[which(dat_db$V1!=""),]
    colnames(dat_db) <- cnames
    dat_db[,grep('Analyte|Total',colnames(dat_db))] <- apply(dat_db[,grep('Analyte|Total',colnames(dat_db))],2,as.numeric)  
    
    if (i==1){
      a <- mgsub(c("(",",",")"), rep(".",3), dat_db$Location, fixed = TRUE)
      Loc_db <- as.data.frame(do.call('rbind', sapply(a,function(x){ strsplit(x,".",fixed=T) }, USE.NAMES=FALSE)))
      well.i <- apply(Loc_db,2,function(x){ all(grepl('A|B|C|D|E|F|G|H',x)) })
      Loc_db <- cbind.data.frame(Location = dat_db$Location, 
                                 SampleNumber = Loc_db[,1],
                                 Well_coord = Loc_db[,well.i])
      Loc_db <- mutate(as.data.frame(Loc_db),
                       Well.Letter = substr(Well_coord,1,1), 
                       Well.Number = as.numeric(substring(Well_coord,2)))
      files <- paste0(plate, '_', dat_db$Location)
      Loc_db <- cbind.data.frame(File=files, Plate=plate, Loc_db)
      Loc_db <- merge(Loc_db, plate.design.db ,by='Well_coord',all.x=TRUE, sort=FALSE)
      rownames(Loc_db) <- as.character(Loc_db$File)
      Loc_db <- mutate(Loc_db[files,c('File','Plate','SampleNumber','Location','Well.Number','Well.Letter','Well_coord','print.plate.order')],
                       Well_coord = factor(Well_coord,levels=as.character(plate.design.db$Well_coord)),
                       Well.Letter = factor(Well.Letter,levels=LETTERS[1:8]),
                       letters_numeric = mapvalues(Well.Letter,LETTERS[1:8],1:8),
                       Well.Number = factor(Well.Number,levels=as.character(1:12)),
                       Plate.Date = plate_date,
                       Plate.Time = plate_time,
                       Plate.TimeHR = plate_timeHR)
      rownames(Loc_db) <- as.character(Loc_db$File)
    }
    
    dat_mat <- t(as.matrix(dat_db[,-c(1,2)]))
    colnames(dat_mat) <- paste0(plate, '_', dat_db$Location)
    dat_mat <- dat_mat[-grep('Total Events', rownames(dat_mat), fixed=TRUE),]
    L[[i]] <- dat_mat
  }
  
  PD <- cbind.data.frame(dat_db[,c('Sample','Location')], filename=fname.red)
  PD <- merge(PD, Loc_db, by="Location", all.x=TRUE)
  rownames(PD) <- as.character(PD$File)
  
  PD$CountSum <- as.vector(colSums(L$Count[,rownames(PD)], na.rm = TRUE))
  PD$CountMean <- as.vector(colMeans(L$Count[,rownames(PD)], na.rm = TRUE))
  PD$CountMin <- as.vector(apply(L$Count[,rownames(PD)],2,function(x){ min(x,na.rm = TRUE) }))
  PD$CountMax <- as.vector(apply(L$Count[,rownames(PD)],2,function(x){ max(x,na.rm = TRUE) }))
  
  L[['pData']] <- PD[colnames(dat_mat),]
  L[['Median']] <- L[['Median']][,rownames(L[['pData']])]
  L[['NetMFI']] <- L[['NetMFI']][,rownames(L[['pData']])]
  L[['Count']] <- L[['Count']][,rownames(L[['pData']])]
  L[['AssayInfo']] <- dat_assay
  
  return(L)
}