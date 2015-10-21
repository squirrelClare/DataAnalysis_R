#2015年7月2日
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

holiday_dayTransWeekday<-function(vDate)
{
  	#一周七天的值配置为：（配置规则为值越小，影响越小；值越大，影响越大）
  	#星期一：0.6
  	#星期二：0.5
  	#星期三：0.4
  	#星期四：0.5
  	#星期五：0.8
  	#星期六：1.5
  	#星期日：1.8
  	#节假日前：2.0
  	#节假日：3.0
  	#节假日后：2.5
  	stdDate=vDate
  	week=weekdays(stdDate)
  	daynums=rep(0,length(vDate))
  	daynums[which(week=='星期一')]=0.6
  	daynums[which(week=='星期二')]=0.5
  	daynums[which(week=='星期三')]=0.4
  	daynums[which(week=='星期四')]=0.5
  	daynums[which(week=='星期五')]=0.8
  	daynums[which(week=='星期六')]=1.5
  	daynums[which(week=='星期日')]=1.8

  	daynums[which(stdDate==as.Date('2014-01-26'))]=0.3
  	daynums[which(stdDate==as.Date('2014-02-08'))]=0.3
  	daynums[which(stdDate==as.Date('2014-05-04'))]=0.3
  	daynums[which(stdDate==as.Date('2014-09-28'))]=0.3
  	daynums[which(stdDate==as.Date('2014-10-11'))]=0.3

  	daynums[which(stdDate==as.Date('2013-01-05'))]=0.3
  	daynums[which(stdDate==as.Date('2013-01-06'))]=0.3
  	daynums[which(stdDate==as.Date('2013-02-16'))]=0.3
  	daynums[which(stdDate==as.Date('2013-02-17'))]=0.3
  	daynums[which(stdDate==as.Date('2013-04-07'))]=0.3
  	daynums[which(stdDate==as.Date('2013-04-27'))]=0.3
  	daynums[which(stdDate==as.Date('2013-04-28'))]=0.3
  	daynums[which(stdDate==as.Date('2013-06-08'))]=0.3
  	daynums[which(stdDate==as.Date('2013-06-09'))]=0.3
  	daynums[which(stdDate==as.Date('2013-09-22'))]=0.3
  	daynums[which(stdDate==as.Date('2013-09-29'))]=0.3
  	daynums[which(stdDate==as.Date('2013-10-12'))]=0.3

  	daynums[which(stdDate==as.Date('2012-01-21'))]=0.3
  	daynums[which(stdDate==as.Date('2012-01-29'))]=0.3
  	daynums[which(stdDate==as.Date('2012-03-31'))]=0.3
  	daynums[which(stdDate==as.Date('2012-04-01'))]=0.3
  	daynums[which(stdDate==as.Date('2012-04-28'))]=0.3
  	daynums[which(stdDate==as.Date('2012-09-29'))]=0.3
  	return(daynums)
  }
  #生成各年份的法定不节假日起始日期和假期天数
  holiday_festivalDB<-function() {
  	festival<-matrix(nrow=0,ncol=4,dimnames=list(NULL,c('year','fType','startdate','ahead')))
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
#获取指定区间内的周末
holiday_weekday<-function(f_date,t_date)
{
	tmp_dates<-holiday_dateSeq(f_date,t_date)
	score<-holiday_dayTransWeekday(tmp_dates)
	dates<-tmp_dates[score%in%c(1.5,1.8)]
	return(dates)
}

#生成指定区间内的日期序列
holiday_dateSeq<-function(f_date,t_date) {
	return(seq(as.Date(f_date), as.Date(t_date), by=1))
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


#获取预测日期中是节假日的日期
holiday_dateOfFestival<-function(startdate,t_date) {
	dates<-seq(as.Date(startdate), as.Date(t_date), by=1)
	isFestival<-dates%in%holiday_festivalDateDetail()$date

	festivalDate<-dates[isFestival]
	return(festivalDate)
}

holiday<-function(f_date,t_date) {
	weekends<-strftime(holiday_weekday(f_date,t_date),format='%Y-%m-%d')
	festivals<-strftime(holiday_dateOfFestival(f_date,t_date),format='%Y-%m-%d')
	resDates<-as.Date(sort(union(weekends,festivals)))
	return(resDates)
}

#获取指定区间内的天气数据
holiday_getWethData<-function(fDate,tDate,wethData) {
	wethData$WETH_DATE<-as.Date(wethData$WETH_DATE)
	tmpDates<-holiday(fDate,tDate)
	tmpWethData<-wethData[which(wethData$WETH_DATE%in%tmpDates),]
	return(tmpWethData)
}

holiday_monthWethData<-function(year,mth,street) {
	sql<-paste0("select * from weather_data where month(WETH_DATE)=",mth,
		" and street='",street,"' and year(WETH_DATE)='",year,"' order by WETH_DATE;")
	wethData<-holiday_exeDm2015(sql)
	fDate<-as.character.Date(min(wethData$WETH_DATE))
	tDate<-as.character.Date(max(wethData$WETH_DATE))
	return(holiday_getWethData(fDate,tDate,wethData))
}

holiday_test<-function() {
	wethData<-read.csv('F:/r_script/福田区_竹子林基地.csv')
	m09<-holiday_monthWethData(year=2013,mth=9,street='竹子林基地')
	m10<-holiday_monthWethData(year=2013,mth=10,street='竹子林基地')
	plot(as.numeric(m09$MAX_TMP),type='l',ylim=c(20,39))
	lines(as.numeric(m10$MAX_TMP),col='red')
	lines(as.numeric(m09$MIN_TMP),lty=2)
	lines(as.numeric(m10$MIN_TMP),col='red',lty=2)
}