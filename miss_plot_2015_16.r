#在数据库HUMBIRD中执行sql，并返回查询的结果
exeHumbird<-function(sql){
	library(RMySQL)
	conn<-dbConnect(RMySQL::MySQL(),dbname='HUMMINGBIRD',username='humbird',host='192.168.10.87',
		port=3306,password='humbird123')
	res<-dbSendQuery(conn,statement=sql)
	mydata<-fetch(res,n=-1)
	dbDisconnect(conn)
	return(mydata)
}


#在数据库DM2015中执行sql，并返回查询的结果
exeDm2015<-function(sql){
	library(RMySQL)
	conn<-dbConnect(RMySQL::MySQL(),dbname='DM2015',username='dimer',host='192.168.10.87',
		port=3306,password='data123')
	dbSendQuery(conn,statement="set names 'utf8';")
	res<-dbSendQuery(conn,statement=sql)
	mydata<-fetch(res,n=-1)
	dbDisconnect(conn)
	return(mydata)
}

plt<-function(id,fdate,tdate) {
	src<-exeDm2015(paste0("SELECT * from JL_TAIQU_LOAD WHERE VKONT='",
		id,"' and LOAD_DATE >='",fdate,"' and LOAD_DATE <='",tdate,"';"))
	src<-src[order(src$LOAD_DATE),]
	src_mat<-as.matrix(src[5:100])
	dates<-src$LOAD_DATE
	weekdays<-weekdays(strptime(dates,format = '%Y-%m-%d'))

	path<-paste0('~/Documents/节假日预测/',id)
	if (!file.exists(path)){
		dir.create(path)
	}

	setwd(path)
	for(i in 1:nrow(src_mat)){
		tiff(file=paste0('',dates[i],'.tiff'), res = 300, width = 2400, height = 2400, compression = "lzw")    
    		#画图
    		
    		top_bot<-c(range(src_mat[i,]),range(src_mat[i,]))
    		rang<-range(top_bot)+c(-50,50)
    		
    		plot(src_mat[i,],type='l',col = 'red',xaxt = 'n',xlim = c(0,100),ylim=rang,xlab = '时点',ylab = '负荷',main=paste0(dates[i],'__',weekdays[i]))
    		lines(src_mat[i,],type='l',col='blue')
    		axis(side =1,at = seq(0,95,4),labels = colnames(src)[2:97][seq(1,96,4)] )
    		legend("bottomright",legend=c('预测','原始'),col=c('red','blue'),lty=1)
    		dev.off()
    	}
    }
# 
# id = '000047500892'
# fdate = '2014-2-15'
# tdate = '2014-5-1'




plt2<-function(id,fdate,tdate) {
  library(dfexplore)
  loadData<-read.csv(file = '~/Desktop/000047500892——1.csv',header = T)
  loadData$X<-NULL
#   loadDate$LOAD_DATE<-as.Date(loadDate$LOAD_DATE)
  loadData$LOAD_DATE<-strftime(strptime(loadData$LOAD_DATE,format = '%Y/%m/%d'),format = '%Y-%m-%d')

  loadData<-loadData[order(loadData$LOAD_DATE),]
  src<-loadData[(loadData$LOAD_DATE)%in%strftime(seq(as.Date(fdate),as.Date(tdate),1),format = '%Y-%m-%d'),]
  
  src_mat<-as.matrix(src[2:97])
  
  
  dates<-as.Date(src$LOAD_DATE)
  weekdays<-weekdays(strptime(dates,format = '%Y-%m-%d'))
  
  path<-paste0('~/Documents/节假日预测/',id)
  if (!file.exists(path)){
    dir.create(path)
  }
  
  setwd(path)
  for(i in 1:nrow(src_mat)){
    tiff(file=paste0('',dates[i],'.tiff'), res = 300, width = 2400, height = 2400, compression = "lzw")    
    plot(src_mat[i,],type='l',col = 'red',xlab = '时点',ylab = '负荷',main=paste0(dates[i],'__',weekdays[i]))
    legend("bottomright",legend=c('预测','原始'),col=c('red','blue'),lty=1)
    dev.off()
  }
}


plt3<-function(id,fdate,tdate,preds) {
  library(dfexplore)
  loadData<-read.csv(file = '~/Desktop/000047500892——1.csv',header = T)
  loadData$X<-NULL
  #   loadDate$LOAD_DATE<-as.Date(loadDate$LOAD_DATE)
  loadData$LOAD_DATE<-strftime(strptime(loadData$LOAD_DATE,format = '%Y/%m/%d'),format = '%Y-%m-%d')
  
  loadData<-loadData[order(loadData$LOAD_DATE),]
  src<-loadData[(loadData$LOAD_DATE)%in%strftime(seq(as.Date(fdate),as.Date(tdate),1),format = '%Y-%m-%d'),]
  
  src_mat<-as.matrix(src[2:97])
  preds<-as.matrix(preds)
  
  dates<-as.Date(src$LOAD_DATE)
  weekdays<-weekdays(strptime(dates,format = '%Y-%m-%d'))
  
  path<-paste0('~/Documents/节假日预测/',id)
  if (!file.exists(path)){
    dir.create(path)
  }
  
  setwd(path)
  for(i in 1:nrow(src_mat)){
    tiff(file=paste0('',dates[i],'_02.tiff'), res = 300, width = 2400, height = 2400, compression = "lzw")    
    plot(src_mat[i,],type='l',col = 'red',xlab = '时点',ylab = '负荷',main=paste0(dates[i],'__',weekdays[i]))
    lines(as.numeric(preds[i,]),col='blue')
    legend("bottomright",legend=c('预测','原始'),col=c('red','blue'),lty=1)
    dev.off()
  }
}

