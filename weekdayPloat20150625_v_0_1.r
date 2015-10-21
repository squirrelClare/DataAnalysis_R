#在数据库HUMBIRD中执行sql，并返回查询的结果
holiday_exeHumbird<-function(sql) {
	library(RMySQL)
	conn<-dbConnect(RMySQL::MySQL(),dbname='HUMMINGBIRD',username='humbird',host='192.168.10.87',
		port=3306,password='humbird123')
	res<-dbSendQuery(conn,statement=sql)
	mydata<-fetch(res,n=-1)
	dbDisconnect(conn)
	return(mydata)
}

# 在数据库DM2015中执行sql，并返回查询的结果
holiday_exeDm2015<-function(sql) {
	library(RMySQL)
	conn<-dbConnect(RMySQL::MySQL(),dbname='DM2015',username='dimer',host='192.168.10.87',
		port=3306,password='data123')
	dbSendQuery(conn,statement="set names 'utf8';")
	res<-dbSendQuery(conn,statement=sql)
	mydata<-fetch(res,n=-1)
	dbDisconnect(conn)
	return(mydata)
}

#生成各年份的法定不节假日起始日期和假期天数
holiday_festivalDB<-function() {
	festival<-matrix(nrow=0,ncol=4,dimnames=list(NULL,c('year','fType','startDay','ahead')))
	festival<-rbind(festival,c(2012,'元旦','2012-1-1',3))
	festival<-rbind(festival,c(2012,'春节','2012-1-22',7))
	festival<-rbind(festival,c(2012,'清明节','2012-4-2',3))
	festival<-rbind(festival,c(2012,'劳动节','2012-4-29',3))
	festival<-rbind(festival,c(2012,'端午节','2012-6-22',3))
	festival<-rbind(festival,c(2012,'中秋节','2012-9-30',1))
	festival<-rbind(festival,c(2012,'国庆节','2012-10-1',7))

	festival<-rbind(festival,c(2013,'元旦','2013-1-1',3))
	festival<-rbind(festival,c(2013,'春节','2013-2-9',7))
	festival<-rbind(festival,c(2013,'清明节','2013-4-4',3))
	festival<-rbind(festival,c(2013,'劳动节','2013-4-29',3))
	festival<-rbind(festival,c(2013,'端午节','2013-6-10',3))
	festival<-rbind(festival,c(2013,'中秋节','2013-9-19',1))
	festival<-rbind(festival,c(2013,'国庆节','2013-10-1',7))

	festival<-rbind(festival,c(2014,'元旦','2014-1-1',3))
	festival<-rbind(festival,c(2014,'春节','2014-1-31',7))
	festival<-rbind(festival,c(2014,'清明节','2014-4-5',3))
	festival<-rbind(festival,c(2014,'劳动节','2014-5-1',3))
	festival<-rbind(festival,c(2014,'端午节','2014-6-2',3))
	festival<-rbind(festival,c(2014,'中秋节','2014-9-8',1))
	festival<-rbind(festival,c(2014,'国庆节','2014-10-1',7))

	festival<-rbind(festival,c(2015,'元旦','2015-1-1',3))
	festival<-rbind(festival,c(2015,'春节','2015-2-18',7))
	festival<-rbind(festival,c(2015,'清明节','2015-4-5',3))
	festival<-rbind(festival,c(2015,'劳动节','2015-5-1',3))
	festival<-rbind(festival,c(2015,'端午节','2015-6-20',3))
	festival<-rbind(festival,c(2015,'中秋节','2015-9-27',1))
	festival<-rbind(festival,c(2015,'国庆节','2015-10-1',7))
	return(festival)
}

#获取预测日期中是节假日的日期
holiday_dateOfFestival<-function(startDay,toDay) {
	dates<-seq(as.Date(startDay), as.Date(toDay), by=1)
	isFestival<-dates%in%holiday_festivalDateDetail()$date

	festivalDate<-dates[isFestival]
	return(festivalDate)
}

#获取全部节假日具体日期信息
holiday_festivalDateDetail<-function() {
	festival<-holiday_festivalDB()
	festivalDetail<-matrix(nrow=0,ncol=4,dimnames=list(NULL,c('year','fType','date','index')))
	for (i in 1:nrow(festival)) {
		tmpYear<-festival[i,1]
		tmpFest<-festival[i,2]
		tmpDate<-as.Date(festival[i,3])
		tmpAhead<-as.numeric(festival[i,4])
		tmpIndex<-1
		tmpDates<-strftime(seq(tmpDate, as.Date(tmpAhead-1,tmpDate), by=1),format="%Y-%m-%d" )

		for(d in tmpDates) {
			festivalDetail<-rbind(festivalDetail,c(tmpYear,tmpFest,strftime(d,),tmpIndex))
			tmpIndex=tmpIndex+1
		}
	}
	festivalDetail<-as.data.frame(festivalDetail)
	festivalDetail$date<-as.Date(festivalDetail$date)
	festivalDetail$year<-as.numeric(as.character(festivalDetail$year))
	return(festivalDetail)
}

#获取指定区间内的周末
holiday_weekdayHoliday<-function(startDay,toDay)
{
	dates<-seq(as.Date(startDay), as.Date(toDay), by=1)
	dates<-dates[weekdays(dates)%in%c('Sunday','Saturday')]
	hDateT<-holiday_dateOfFestival(startDay,toDay)
	hDateL<-holiday_sameDayLyear(hDateT)
  print(hDateT)
  	dates<-c(dates,hDateT,hDateL)
	return(unique(dates))
}

#获取前一年统一日期
holiday_sameDayLyear<-function(current_day) {
	festivalDetail<-holiday_festivalDateDetail()
	tmp<-festivalDetail[which(festivalDetail$date%in%current_day),]
  	
  	dates<-c()
  	for(i in 1:nrow(tmp)) {

  		year<-tmp[i,]$year
		fType<-tmp[i,]$fType
		index<-tmp[i,]$index
		tmpLyear<-festivalDetail[which(festivalDetail$year==(year-1) & festivalDetail$fType==fType & festivalDetail$index==index)  ,]
		dates<-c(dates,tmpLyear$date)
  	}
	return(dates)
}

weekdayPlot<-function(startDay='2014-8-1',toDay='2014-9-30',id) {
	dates<-holiday_weekdayHoliday(startDay,toDay)
#   print(dates)
  	sqlSrc<-paste0("select * from JL_TAIQU_LOAD where LOAD_DATE in ('",paste(dates,sep = '',collapse = "','"),
  		"') and VKONT='",id,"' order by LOAD_DATE;")
  	srcData<-holiday_exeDm2015(sqlSrc)

  	dates<-srcData$LOAD_DATE
    	srcMat<-as.matrix(srcData[5:100])

  	nr<-nrow(srcData)
  	path<-paste0('~/Desktop/holiday_2014_08&09/',id)
  	if (!file.exists(path)){
    	dir.create(path)
  	}
  	setwd(path)

  	for (i in 1:nr) {
  		# rang<-c(min(min(srcMat[i,]),min(srcMat[i,])),max(max(srcMat[i,]),max(srcMat[i,])))+c(-50,50)


  		tiff(file=paste0(id,'__',dates[i],'.tiff'), res = 300, width = 2400, height = 2400, compression = "lzw")
  		plot(srcMat[i,],type='l',col='blue',xaxt = 'n',xlim = c(0,100),xlab = '时点',ylab = '负荷')
  		# legend("bottomright",legend=c('预测','原始'),col=c('red','blue'),lty=1)
  		dev.off()
  	}


}

pointPlt<-function(startDay='2014-8-1',toDay='2014-9-30',id) {
	dates<-holiday_weekdayHoliday(startDay,toDay)
  
  	sqlSrc<-paste0("select * from JL_TAIQU_LOAD where LOAD_DATE in ('",paste(dates,sep = '',collapse = "','"),
  		"') and VKONT='",id,"' order by LOAD_DATE;")
  	srcData<-holiday_exeDm2015(sqlSrc)

  	dates<-srcData$LOAD_DATE
    	srcMat<-as.matrix(srcData[5:100])

  	nc<-ncol(srcMat)
  	path<-paste0('~/Desktop/holiday_2014_08&09_point/',id)
  	if (!file.exists(path)){
    	dir.create(path)
  	}
  	setwd(path)

  	for (i in 1:nc) {
  		# rang<-c(min(min(srcMat[i,]),min(srcMat[i,])),max(max(srcMat[i,]),max(srcMat[i,])))+c(-50,50)


  		tiff(file=paste0(id,'__c0',i,'_.tiff'), res = 300, width = 2400, height = 2400, compression = "lzw")
  		plot(srcMat[,i],type='l',col='red',xaxt = 'n',xlab = '日期',ylab = '负荷')
  		# legend("bottomright",legend=c('预测','原始'),col=c('red','blue'),lty=1)
  		dev.off()
  	}
}

wethPlt<-function(startDay='2014-8-1',toDay='2014-9-30') {
	dates<-holiday_weekdayHoliday(startDay,toDay)
  
	wethData<-read.csv("~/Desktop/福田区_竹子林基地.csv",header=T)
	wethData$WETH_DATE<-as.Date(wethData$WETH_DATE)

	wethData<-wethData[which(wethData$WETH_DATE%in%dates),]
  plot(wethData$MIN_TMP,type = 'l',col='blue',ylim = c(0,40))
  lines(wethData$MAX_TMP,col='red',type = 'l')
  lines(wethData$MAX_TMP-wethData$MIN_TMP,type = 'l',col='green')
}

startDay<-'2014-8-1'
toDay<-'2014-9-15'
wethData=read.csv("~/Desktop/福田区_竹子林基地.csv",header=T)
