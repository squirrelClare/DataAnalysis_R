#相关包载入
library(amap)
library(rattle)
library(fpc)


#功能：
#     获取格式化后的数据
#参数：
#     path:csv格式的文件所在路径
#返回：
#     mydata:格式化后的数据框
data_format<-function(path){
  src_data<-read.csv(path)
  mydata<-src_data[3:98]
  LOAD_DATE<-strptime(levels(src_data$LOAD_DATE),format='%Y-%m-%d')
  mydata$tag<-weekdays(LOAD_DATE)
  return(mydata)
}

#功能：
#     画出一个台区全部时点数据
#参数：
#     src:台区时点数据集
#返回：
#     无返回值
dplt<-function(src){
  colors<-sample(colors(),96)
  plot(x = unlist(src[1]),type = 'l',col = colors[1],ylim=c(0,500))
  for(i in 2:96){
    lines(unlist(src[i]),type = 'l',col = colors[i],ylim=c(0,500))
  }
}

#功能：
#     对聚类后个类图像画图默认图片存放路径D:\\test
#参数：
#     groups:分组数据
#     path:图像存储路径
#返回：
#     无返回值
pltcluster<-function(groups,path){ 
  setwd(path)
  #   tiff(file='聚为9类.tiff', res = 300, width = 2400, height = 2400, compression = "lzw")
  #   par(mfrow = c(3,3))
  for(i in 1:length(groups)){
    tiff(file=paste0('类别',i,'.tiff'), res = 300, width = 2400, height = 2400, compression = "lzw")
    
    tmp<-as.data.frame(groups[i])
    n=nrow(tmp)
    colors<-sample(colors(),size = n)
    
    #画图
    plot(unlist(tmp[1,]),type ="l",col = colors[1],ylim = c(-3,3),main=paste0('类别',i),xlab='时点',ylab='幅值')
    for(j in 2:n){
      lines(unlist(tmp[j,]),col=colors[j])
    }
    dev.off()
  }
  #   dev.off()
}

#功能：
#     对聚类后各类统计偏差情况，欧式距离除以一条记录中点的个数
#参数：
#     groups:分组数据
#     n_point:一条记录中点的个数
#返回：
#     无返回值
bias<-function(groups,n_point){
  biases<-lapply(groups,dist)
  summarys<-lapply(biases,summary)
  summarys<-matrix(unlist(summarys),ncol = 6,byrow = TRUE)
  summarys<-as.data.frame(summarys)/n_point
  print(n_point)
  names(summarys)<-c('Min.','1st Qu.','Median','Mean 3rd','Qu.','Max.')
  return(summarys)
}

#功能：
#     对聚类后各类统计偏差情况，欧式距离除以一条记录中点的个数
#参数：
#     path:数据所在路径
#     k:类的个数
#返回：
#     无返回值
data_manage<-function(path,k=9){
  src_data<-data_format(path)
#   mydata<-as.data.frame(t(scale(src_data[1:96])))
  mydata<-as.data.frame(t(src_data[1:96]))
  chcluster<-hclusterpar(x=mydata)
  # plot(chcluster)
  centers<-centers.hclust(x=mydata,h=chcluster,nclust=3)
  
  par(bg='grey')
  plot(chcluster,main='',sub='',xlab='',hang=-1)
  rect.hclust(tree=chcluster,k)
  
  groups = split(mydata,f = cutree(chcluster,h=3))#数据分组
  pltcluster(groups,"~/Documents/cluster_2/")#画出聚类后的图像
  
  
  #统计聚类分布情况
  class_id<-cutree(chcluster,h=3)
  biases<-bias(groups,dim(src_data)[1])
  #输出偏差情况
  print(class_id)
}
data_manage('~/Desktop/S4_000047011966.csv',15)
