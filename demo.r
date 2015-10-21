# 在数据库DM2015中执行sql，并返回查询的结果
holiday_exeDm2015<-function(sql) {
  library(RMySQL)
  conn<-dbConnect(RMySQL::MySQL(),dbname='DM2015',username='dimer',host='192.168.10.87',
                  port=3306,password='data123')
  res<-dbSendQuery(conn,statement=sql)
  mydata<-fetch(res,n=-1)
  dbDisconnect(conn)
  return(mydata)
}

sql<-"select * from JL_TAIQU_LOAD where STREET_WORKSTATION_NAME='福田' and LOAD_DATE='2013-12-16';"

src<-holiday_exeDm2015(sql)
loadData<-src[5:100]
loadData<-as.matrix.data.frame(loadData)
id<-src$VKONT
path<-paste0('C:/Users/dell/Desktop/20131216/')
if (!file.exists(path)){
  dir.create(path)
}
setwd(path)
for(i in 1:nrow(loadData)){
  tiff(file=paste0(id[i],'.tiff'), res = 300, width = 2400, height = 2400, compression = "lzw")    
  #画图
  
  plot(loadData[i,],type='l',col = 'red',xaxt = 'n',xlim = c(0,100),xlab = '时点',ylab = '负荷',main=paste0(id[i]))
  dev.off()
}

loadData=read.csv("C:/Users/dell/Desktop/000047001817.csv",header=T)
loadData$X=NULL
loadData$LOAD_DATE=as.Date(loadData$LOAD_DATE)
wethData=read.csv("C:/Users/dell/Desktop/福田区_竹子林基地.csv",header=T)
wethData$X=NULL
wethData$WETH_DATE=as.Date(wethData$WETH_DATE)
holiday_init()
models=holiday_buildModel(loadData,wethData,startdate="2012-09-01",enddate="2013-10-31")
#preds=prediction(models,loadData,wethData,predate="2014-09-15",ahead=7)
preds=holiday_prediction(models,loadData,wethData,predate="2014-04-30",ahead=7)$preds.modify

#计算综合系数
holiday_globalCoef<-function(temp_hist,temp_pre,date_hist,date_pre,n_top,alpha,beta_1,beta_2=.95) {
  temp_hist<-holiday_wethDataFormat(temp_hist)
  holiday_dateCorr<-holiday_dateCorr(date_hist,date_pre,beta_1,beta_2)
  holiday_tempCorr<-holiday_tempCorr(temp_hist,temp_pre,alpha)
  global<-holiday_dateCorr*holiday_tempCorr
  global<-global[order(global,decreasing = TRUE)]
  return(head(x = global,n = n_top))
}
#计算历史日的温度数据与预测日的温度关联度
holiday_tempCorr<-function(temp_hist,temp_pre,alpha=0.5) {
  tmp<-lapply(temp_hist,FUN = function(x) {return(abs(x-temp_pre))})
  tmp<-as.data.frame(tmp)
  tmp_scale<-apply(tmp,1,FUN=function(x) {return((x-min(x))/(max(x)-min(x)))})
  res<-(min(tmp_scale)+alpha*max(tmp_scale))/(tmp_scale+alpha*max(tmp_scale))
  return(apply(res,MARGIN = 1,sum)/2)
}

#计算时间因子匹配系数
holiday_dateCorr<-function(dates,date_pre,beta_1=.90,beta_2=.95) {
  n_day<-length(dates)
  dates<-strptime(dates,format='%Y-%m-%d')
  t<-strptime(date_pre,format = '%Y-%m-%d')-dates

  S<-rep(0,n_day)
  S[which(dates==strptime(holiday_sameDayLyear(date_pre),format='%Y-%m-%d'))]<-1
  delta<-beta_1**((1-S)*as.numeric(floor(t/7)))*beta_2**(S*as.numeric(floor(t/340)))
  names(delta)<-strftime(dates,format='%Y-%m-%d')
  return(delta)
}
