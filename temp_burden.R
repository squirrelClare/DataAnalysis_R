#提取指定提起的气温数据
get_weath<-function(path='~/Desktop/福田区_梅林水库.csv'){
  src<-read.csv(file = path)
  src$WETH_DATE<-strptime(src$WETH_DATE,format = '%Y/%m/%d')
  return(src)
}

#提取指定编号的负荷数据
get_burden<-function(id){
  sql<-paste0("select * from JL_TAIQU_LOAD where VKONT='",id,"' order by LOAD_DATE;")
  library(RMySQL)
  conn<-dbConnect(RMySQL::MySQL(),dbname='DM2015',username='dimer',host='192.168.10.87',
                  port=3306,password='data123')
  
  res<-dbSendQuery(conn,statement=sql)
  mydata<-fetch(res,n=-1)
  dbDisconnect(conn)
  mydata$LOAD_DATE<-strptime(x = mydata$LOAD_DATE,format = '%Y-%m-%d')
  return(mydata)
}

#从原始负荷数据和温度数据中提取公共日期部分数据
data_format<-function(id='000047001601'){
  weath_frame<-get_weath()
  burden_frame<-get_burden(id)
  
  date_max<-min(max(weath_frame$WETH_DATE),max(burden_frame$LOAD_DATE))
  date_min<-max(min(weath_frame$WETH_DATE),min(burden_frame$LOAD_DATE)) 
  
  burden_frame_new<-burden_frame[burden_frame$LOAD_DATE>=date_min &burden_frame$LOAD_DATE<=date_max, ]
  weath_frame_new<-weath_frame[weath_frame$WETH_DATE%in%burden_frame_new$LOAD_DATE, ]
  
  burden_frame_new$weekday<-weekdays(burden_frame_new$LOAD_DATE)
  weath_frame_new$weekday<-weekdays(weath_frame_new$WETH_DATE)
  return(list(weath_frame_new,burden_frame_new))
}

#将天气数据和负荷数据数据归一化
data_scale<-function(data_list){
  weather<-data_list[[1]]
  burden<-data_list[[2]]
  
  weather_mat<-as.matrix(weather[3:7])
  rownames(weather_mat)<-strftime(weather[[2]],format = '%Y-%m-%d')

  burden_mat<-as.matrix(burden[5:100])
  rownames(burden_mat)<-strftime(burden[[4]],format = '%Y-%m-%d')
  
  weather[3:7]<-as.data.frame(scale(weather_mat))
  burden[5:100]<-as.data.frame(scale(burden_mat))
  return(list(weather,burden))
}

split_by_weekday<-function(data_list){
  weather<-data_list[[1]]
  burden<-data_list[[2]]
  weathers<-split(weather,weather$weekday)
  burdens<-split(burden,burden$weekday)
  return(list(weathers,burdens))
}

#画温度数据和试点数据归一化之后的图像
plt<-function(data_list){
  weather_mat<-data_list[[1]]
  burden_mat<-data_list[[2]]
  plot(scale(burden_mat[,1]),type='l',col='red')
  lines(scale(weather_mat[,1]),col='green',type='l')

}

cor_per_point<-function(weather,burden){
  covs<-lapply(5:100,FUN=function(i){return(cor(weather$MAX_TMP,burden[[i]]))})
  return(covs)
}
#计算各时点数据按周一到周日划分后与温度的相关性
cor_comput<-function(data_list){
  weathers<-data_list[[1]]
  burdens<-data_list[[2]]
  rowname<-unlist(lapply(weathers,FUN=function(weather)
  {
    return(weather$weekday[1])
    }))
  
  cor_mat<-matrix(nrow = 7,ncol = 96)
  rownames(cor_mat)<-rowname
  
  for (i in 1:7) {
    tmp_weather<-weathers[[i]]
    cor_mat[i,]<-unlist(cor_per_point(tmp_weather,burdens[[i]]))
  }
  return(cor_mat)
}




main<-function(id){
  src_data<-data_format(id)#读入初始数据
  scale_data<-data_scale(src_data)#数据据归一化
  data_list<-split_by_weekday(data_list = scale_data)
  result<-cor_comput(data_list = data_list)
  colors<-sample(colors(),size = 7)
  pchs<-sample(0:25,size = 7)
  
  path<-paste0('~/Documents/cor_',id)
  if (!file.exists(path)){
    dir.create(path)
  }
  setwd(path)
  
  tiff(file=paste0(id,'__时点与温度相关系数图（未按星期划分）.tiff'), res = 300, width = 2400, height = 2400, compression = "lzw")    
  plot(unlist(cor_per_point(scale_data[[1]],scale_data[[2]])),type='p',pch=13,col='red',ylim = c(0,1),xlim = c(0,97),
       main='时点与温度的相关系数',xlab = '时点',ylab = '相关系数')
  lines(rep(0.6,96))
  dev.off()
  
  for (i in 1:7) {
    name=rownames(result)[i]
    tiff(file=paste0(id,'__',name,'.tiff'), res = 300, width = 2400, height = 2400, compression = "lzw")    
    plot(result[i,],type='p',pch=pchs[i],col='red',ylim = c(0,1),xlim = c(0,97),
         main=paste0(name,'时点与温度的相关系数'),xlab = '时点',ylab = '相关系数')
    lines(rep(0.6,96))
    dev.off()
  } 
  
  
  tiff(file=paste0(id,'__汇总.tiff'), res = 300, width = 2400, height = 2400, compression = "lzw")    
  plot(unlist(cor_per_point(scale_data[[1]],scale_data[[2]])),type='l',col='red',ylim = c(0,1),xlim = c(0,97),
       main='时点与温度的相关系数',lwd=4,xlab = '时点',ylab = '相关系数')
  lines(rep(0.6,96))
  
  for (i in 1:7) {
    name=rownames(result)[i]
    lines(result[i,],col=colors[i])
  } 
  legend("bottomright",legend=c('main',rownames(result)),col=c('red',colors),lty=1)
  dev.off()
  
}