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
  tmp<-src_data[,3:98]
  mydata<-as.data.frame(t(scale(t(tmp))))#数据标准化
  LOAD_DATE<-strptime(src_data$LOAD_DATE,format='%Y/%m/%d')
  mydata$tag<-weekdays(LOAD_DATE)
  rownames(mydata)<-src_data$LOAD_DATE
  return(mydata)
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
    m=ncol(tmp)
    n=nrow(tmp)
    tmp<-tmp[,-m]
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
#     对指定台区数据进行聚类分析
#参数：
#     path:数据所在路径
#     k:聚类数，默认为9
#返回：
#     无返回值
data_manage<-function(path,k=9){
  #读取标准化后的数据
  mydata<-data_format(path)
  data_cluster<-mydata[,-97]#去掉星期列
  
  #聚类
  chcluster<-hclusterpar(x=data_cluster,link='ward')#欧氏距离，利差平方和聚类
  # plot(chcluster)
  centers<-centers.hclust(x=data_cluster,h=chcluster,nclust=k)#聚类中心
  groups = split(mydata,f = cutree(chcluster,k))#数据分组
  pltcluster(groups,"D:\\test")#画出聚类后的图像
  
  #统计聚类分布情况
  class_id<-cutree(chcluster,k)
  data_div<-data.frame(class_id,mydata$tag)#聚类后各类的分布图
  class_table<-table(data_div)
  write.csv(x = class_table,file = 'D:\\test\\table.csv')
  
  biases<-bias(groups,96)
  print(biases)
}

#功能：
#     对聚类后各类统计偏差情况，欧式距离除以一条记录中点的个数
#参数：
#     path:数据所在路径
#     k:类的个数
#返回：
#     无返回值

bias<-function(groups,n_point){
  biases<-lapply(groups,dist)
  summarys<-lapply(biases,summary)
  summarys<-matrix(unlist(summarys),ncol = 6,byrow = TRUE)
  summarys<-as.data.frame(summarys)/n_point
  names(summarys)<-c('Min.','1st Qu.','Median','Mean 3rd','Qu.','Max.')
  return(summarys)
}

data_manage('C:\\Users\\dell\\Desktop\\000047021626.csv',120)