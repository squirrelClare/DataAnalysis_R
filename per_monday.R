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
#     画数据集中指定点的箱线图
#参数：
#     p:时间点，如C001,C003
#     src_data:数据集
#返回：
#     无
boxplt<-function(p,src_data){
  rb<-boxplot(unlist(src_data[p])~src_data$tag,horizontal = TRUE,notch = TRUE,col = 'bisque')
  
  mn.t <- tapply(unlist(src_data[p]),src_data$tag, mean)
  sd.t <- tapply(unlist(src_data[p]),src_data$tag, sd)
  xi <- 0.3 + seq(rb$n)
  points(mn.t,xi, col = "orange",pch = 18)
  arrows(mn.t - sd.t,xi,mn.t + sd.t,xi, code = 3, col = "pink", angle = 75, length = .1)
}

#功能：
#     画出数据的走势图
#参数：
#     data_list:list型数据
#返回：
#     无返回值
lplt<-function(data_list){
  colors<-sample(colors(),length(data_list))
  
  plot(unlist(data_list[1]),col=colors[1])
  for(i in 2:length(data_list)){
    lines(unlist(data_list[i]),col=colors[i])
  }
}

bias<-function(groups,class_id){
  biases<-lapply(groups,dist)
  summarys<-lapply(biases,summary)
  summarys<-matrix(unlist(summarys),ncol = 6,byrow = TRUE)
  summarys<-as.data.frame(summarys)/table(class_id)
  names(summarys)<-c('Min.','1st Qu.','Median','Mean 3rd','Qu.','Max.')
  return(summarys)
}


myplt<-function(src,tag){
  data=split(src,tag)
  lplt(data)
}

mydata<-data_format('C:\\Users\\dell\\Desktop\\wavesplit_s4\\S4\\S4_000047001600.csv')
myplt(mydata$C084,mydata$tag)
