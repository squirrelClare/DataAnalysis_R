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

#获取前一年统一日期
holiday_sameDayLyear<-function(current_day) {
	festivalDetail<-holiday_festivalDateDetail()
	tmp<-festivalDetail[which(festivalDetail$date==current_day),]

	year<-tmp$year
	fType<-tmp$fType
	index<-tmp$index
	tmpLyear<-festivalDetail[which(festivalDetail$year==(year-1) & festivalDetail$fType==fType & festivalDetail$index==index)  ,]
	return(tmpLyear$  date)
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

main<-function() {
	sqlPre<-"select * from holiday_20140501_0503 where LOAD_DATE='2014-05-01' order by id;"
	preData<-holiday_exeDm2015(sqlPre)
	IDS<-unique(preData$id)
	
	sqlSrc<-paste0("select * from JL_TAIQU_LOAD where VKONT in ('",paste(IDS,sep = '',collapse = "','"),
		"') and LOAD_DATE='2014-05-01' order by VKONT;")
	srcData<-holiday_exeDm2015(sqlSrc)
	  if(length(srcData$VKONT)!=length(preData$id)) {
	    preData<-preData[which(preData$id %in% srcData$VKONT),]
	  }

	preMat<-as.matrix.data.frame(preData[3:98])
	srcMat<-as.matrix.data.frame(srcData[5:100])
	nr<-nrow(srcData)

	path<-paste0('~/Desktop/holidayStatic_20140501/')
	if (!file.exists(path)){
		dir.create(path)
	}
	setwd(path)

	tiff(file=paste0('1.tiff'), res = 300, width = 2400, height = 2400, compression = "lzw")
	res<-matrix(abs(as.numeric(srcMat)-as.numeric(preMat)),nrow = nr)
	ratioMat<-res/srcMat
	rc<-cut(as.numeric(ratioMat),breaks = seq(0, 1, .05))
	barplot(prop.table(table(rc)),ylim=c(0,.60),xlab = '误差率',ylab='百分比',main="2014-09-08负荷预测误差分布图(1)")
	dev.off()

	tiff(file=paste0('2.tiff'), res = 300, width = 2400, height = 2400, compression = "lzw")
	resSum<-apply(res,MARGIN = 1,sum)
	ratio<-resSum/apply(srcMat,1,sum)
	rc<-cut(as.numeric(ratio),breaks = seq(0, 1, .05))
	barplot(prop.table(table(rc)),ylim=c(0,.60),xlab = '误差率',ylab='百分比',main="2014-09-08负荷预测误差分布图(2)")
	dev.off()
}

plt<-function() {
	sqlPre<-"select * from holiday_20140602_0604 order by id;"
	preData<-holiday_exeDm2015(sqlPre)
	IDS<-preData$id
	
	sqlSrc<-paste0("select * from JL_TAIQU_LOAD where VKONT in ('",paste(IDS,sep = '',collapse = "','"),
		"') and LOAD_DATE='2014-06-02' order by VKONT;")
	srcData<-holiday_exeDm2015(sqlSrc)

	if(length(srcData$VKONT)!=length(preData$id)) {
	    	preData<-preData[which(preData$id %in% srcData$VKONT),]
	}

	preMat<-as.matrix.data.frame(preData[3:98])
	srcMat<-as.matrix.data.frame(srcData[5:100])
	nr<-nrow(srcData)

	path<-paste0('~/Desktop/holidayStatic_20140602/')
	if (!file.exists(path)){
		dir.create(path)
	}
	setwd(path)

	for (i in 1:nr) {
		rang<-c(min(min(srcMat[i,]),min(srcMat[i,])),max(max(srcMat[i,]),max(srcMat[i,])))+c(-50,50)


		tiff(file=paste0(IDS[i],'.tiff'), res = 300, width = 2400, height = 2400, compression = "lzw")
		plot(preMat[i,],type='l',col='red',xaxt = 'n',ylim=rang,xlim = c(0,100),xlab = '时点',ylab = '负荷')
		lines(srcMat[i,],col='blue')
		legend("bottomright",legend=c('预测','原始'),col=c('red','blue'),lty=1)
		dev.off()
	}
}