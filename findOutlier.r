holiday_hitIndex <- function (mid, k_size){
  r<-floor(k_size/2)
  return(seq(mid-r,  mid+r,  1))
}

holiday_morphClose <- function (src, k_size){
  dst<-src
  r<-floor(k_size/2)
  dst[(r+1):(length(dst)-r)]<-sapply((r+1):(length(dst)-r), FUN=function(x){return(max(src[holiday_hitIndex(x, k_size)]))})
  dst[(r+1):(length(dst)-r)]<-sapply((r+1):(length(dst)-r), FUN=function(x){return(min(dst[holiday_hitIndex(x, k_size)]))})
  return(dst)
}

holiday_morphOpen <- function (src, k_size){
  dst<-src
  r<-floor(k_size/2)
  dst[(r+1):(length(dst)-r)]<-sapply((r+1):(length(dst)-r), FUN=function(x){return(min(src[holiday_hitIndex(x, k_size)]))})
  dst[(r+1):(length(dst)-r)]<-sapply((r+1):(length(dst)-r), FUN=function(x){return(max(dst[holiday_hitIndex(x, k_size)]))})
  return(dst)
}

holiday_loadDataFilter <- function (loadData, k_size){
  for (i in 1:nrow(loadData)) {
    tmp<-holiday_morphClose(as.numeric(loadData[i, ][2:97]), k_size)
    loadData[i, ][2:97]<-holiday_morphOpen(tmp, k_size)
  } 
  return(loadData)
}

plt <- function() {
  mydata <- read.csv('D:/loadmonitoring_forecast_result.csv')
  mydata <- mydata[2:97][1:100,]
  mydata <- as.matrix.data.frame(mydata)
  diffMat <- apply(mydata, MARGIN = 1, FUN = diff, differences = 1)
  diffMat2 <- apply(mydata, MARGIN = 1, FUN = diff, differences = 2)
  
  
  setwd('D:/异常/1/')
  for (i in 1:100) {
    tiff(file=paste(as.character(i),'.tiff'), res = 300, width = 2400, height = 2400, compression = "lzw")
    plot(mydata[i,], type = 'l')
    dev.off()
  }
  
  
  setwd('D:/异常/2/')
  for (i in 1:100) {
    tiff(file=paste(as.character(i),'.tiff'), res = 300, width = 2400, height = 2400, compression = "lzw")
    plot(diffMat[ ,i], type = 'l')
    dev.off()
  }
  
  setwd('D:/异常/3/')
  for (i in 1:100) {
    tiff(file=paste(as.character(i),'.tiff'), res = 300, width = 2400, height = 2400, compression = "lzw")
    plot(diffMat2[ ,i], type = 'l')
    dev.off()
  }  
}
