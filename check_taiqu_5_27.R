#相关包载入
library(amap)
library(rattle)
library(fpc)


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




#在数据库DM2015中执行sql，并返回查询的结果
exe_dm2015<-function(sql){
  library(RMySQL)
  conn<-dbConnect(RMySQL::MySQL(),dbname='DM2015',username='dimer',host='192.168.10.87',
                  port=3306,password='data123')
  
  res<-dbSendQuery(conn,statement=sql)
  mydata<-fetch(res,n=-1)
  dbDisconnect(conn)
  return(mydata)
}

plt<-function(group){ 
  n=ncol(group)
  colors<-sample(colors(),size = n)
  
  #画图
  plot(group[[1]],type ="l",col = colors[1],xlab='时点',ylab='幅值',ylim = c(100,550))
  for(j in 2:n){
     lines(group[[j]],col=colors[j])
  }
}

#功能：
#     获取格式化后的数据
#参数：
#     path:csv格式的文件所在路径
#返回：
#     mydata:格式化后的数据框
data_format<-function(id){
  sql<-paste0("SELECT * from JL_TAIQU_LOAD WHERE VKONT='",id,"';")
  src_data<-exe_dm2015(sql)
  mydata<-src_data[5:100]
  LOAD_DATE<-strptime(src_data$LOAD_DATE,format='%Y-%m-%d')
  mydata$tag<-weekdays(LOAD_DATE)
  return(mydata)
}


main<-function(){
  sql<-"SELECT * from JL_TAIQU_LOAD WHERE VKONT='000047011966';"
  src<-exe_dm2015(sql)
  src<-src[order(src$LOAD_DATE),]
  data<-src[5:28][c(691:751),]
  plt(data)
  
  id<-'000047011966'
  mydata<-data_format(id)
  mydata<-as.data.frame(t(scale(mydata[1:96])))
  
  chcluster<-hclusterpar(x=mydata)
  # plot(chcluster)
  centers<-centers.hclust(x=mydata,h=chcluster,nclust=3)
  
  par(bg='grey')
  plot(chcluster,main='',sub='',xlab='',hang=-1)
  rect.hclust(tree=chcluster,k=11)
  
  groups = split(mydata,f = cutree(chcluster,h = 3))#数据分组
  pltcluster(groups,"~/Documents/cluster")#画出聚类后的图像
  
  
  #统计聚类分布情况
  class_id<-cutree(chcluster,k)
  biases<-bias(groups,dim(src_data)[1])
  #输出偏差情况
  print(biases)
  
}