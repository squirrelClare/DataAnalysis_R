hitIndex<-function(mid,k_size)
{
  r<-floor(k_size/2)
  return(seq(mid-r, mid+r, 1))
}

morphClose<-function(src,k_size)
{
  dst<-src
  r<-floor(k_size/2)
  dst[(r+1):(length(dst)-r)]<-sapply((r+1):(length(dst)-r),FUN=function(x){return(max(src[hitIndex(x,k_size)]))})
  dst[(r+1):(length(dst)-r)]<-sapply((r+1):(length(dst)-r),FUN=function(x){return(min(dst[hitIndex(x,k_size)]))})
  return(dst)
}

morphOpen<-function(src,k_size)
{
  dst<-src
  r<-floor(k_size/2)
  dst[(r+1):(length(dst)-r)]<-sapply((r+1):(length(dst)-r),FUN=function(x){return(min(src[hitIndex(x,k_size)]))})
  dst[(r+1):(length(dst)-r)]<-sapply((r+1):(length(dst)-r),FUN=function(x){return(max(dst[hitIndex(x,k_size)]))})
  return(dst)
}

loadDataFilter<-function(loadData,k_size)
{
  for (i in 1:nrow(loadData)) 
  {
    tmp<-morphClose(as.numeric(loadData[i,][5:100]),k_size)
    loadData[i,][5:100]<-morphOpen(tmp,k_size)
  }
  return(loadData)
}
