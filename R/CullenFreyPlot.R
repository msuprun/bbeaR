CullenFreyPlot <- function(mat, filename="./CullenFrey.", boot=100, 
                           qqPlot=FALSE, scalePeptide=TRUE) {
  require(fitdistrplus)
  
  if (scalePeptide) {
    adj<-t(apply(mat,1,function(x){x-mean(x,na.rm=T)}))
    mat<-colMeans(adj,na.rm=T)
  } 
  dt<-as.vector(mat)
  dt<-dt[!is.na(dt)]
  
  
  descdist(dt, boot=boot)
  dev.copy(pdf,file=paste0(filename,".pdf",sep=""),onefile=TRUE)
  dev.off()
  # histogram
  hist(dt,col = "cornsilk1", border = "black", prob = TRUE,
       xlab = "Signal Intensity",main = "Histogram")
  lines(density(dt),lwd = 1.5,col = "chocolate3")
  abline(v = mean(dt),col = "royalblue", lwd = 2)
  abline(v = median(dt),col = "red",lwd = 2)
  legend(x = "topright", c("Density", "Mean", "Median"),
         col = c("chocolate3", "royalblue", "red"),lwd = c(2, 2, 2))
  if (qqPlot==TRUE) {
    # QQ plot
    qqnorm(dt, main = "QQ plot")
    qqline(dt)
  }
  dev.copy(pdf,file=paste0(filename,".Historam.pdf",sep=""),onefile=TRUE)
  dev.off()
}
