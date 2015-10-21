#获取指定区间内的周末
holiday_weekday<-function(f_date,t_date)
{
	tmp_dates<-holiday_dateSeq(f_date,t_date)
	score<-holiday_dayTransWeekday(tmp_dates)
	dates<-tmp_dates[score%in%c(1.5,18)]
	return(dates)
}

holiday_init <- function(){
	return(0)
}
holiday_buildModel<-function(loadData,wethData,startdate,enddate)
{
	return(0)
}

holiday_prediction<-function(model,loadData,wethData,predate,ahead,alpha=.5,beta_1=-.95,beta_2=.92)
{	

	loadData<-holiday_SeqKNN(loadData)

	festivalDates<-holiday_dateOfFestival(predate,ahead)
	if(length(festivalDates)==0) {
		return(NULL)
	}


	predate<-festivalDates[1]
	ahead<-length(festivalDates)
	
	loadData$LOAD_DATE<-strftime(loadData$LOAD_DATE,format = '%Y-%m-%d')
	wethData$WETH_DATE<-strftime(wethData$WETH_DATE,format = '%Y-%m-%d')
	predictLoads<-matrix(nrow=ahead,ncol=96)
	colnames(predictLoads)<-colnames(loadData)[2:97]
	data<-holiday_dataSplit(loadData,wethData,predate,ahead)


	for (i in 1:ahead) {
		temp_hist<-rbind(data$pr_h_weather_t_year,data$h_weather_l_year[i,])

		tmp_pre_t_date<-data$h_weather_t_year$WETH_DATE[i]
		tmp_pre_t_weth<-t(data$h_weather_t_year[i,][c('MIN_TMP','MAX_TMP')])
		tmp_pre_l_date<-data$h_weather_l_year$WETH_DATE[i]
		tmp_pre_l_weth<-t(data$h_weather_l_year[i,][c('MIN_TMP','MAX_TMP')])
	    	#系数计算结果
	    	date_and_coef<-holiday_globalCoef(temp_hist,tmp_pre_t_weth,temp_hist$WETH_DATE,tmp_pre_t_date,5,alpha,beta_1,beta_2)
   		 date_and_coef<-date_and_coef/sum(date_and_coef)
    
	    	k_1<-holiday_globalCoef(data$pr_h_weather_l_year,tmp_pre_t_weth,data$pr_h_weather_l_year$WETH_DATE,tmp_pre_t_date,1,alpha,beta_1,beta_2)
	    	k_2<-holiday_globalCoef(data$pr_h_weather_l_year,tmp_pre_l_weth,data$pr_h_weather_l_year$WETH_DATE,tmp_pre_l_date,1,alpha,beta_1,beta_2)
	    	tmp_h_l_load<-holiday_loadData(data$h_load_l_year,tmp_pre_l_date)
	    	sim_k1_load<-holiday_loadData(data$pr_h_load_l_year,names(k_1))
	    	sim_k2_load<-holiday_loadData(data$pr_h_load_l_year,names(k_2))
	    
	    	k<-(tmp_h_l_load+sim_k1_load-sim_k2_load)/tmp_h_l_load
	    	lambda<-holiday_lambdaCoef(loadData,tmp_pre_t_date,tmp_pre_l_date)
	    
	    	sim_date<-names(date_and_coef)
	    	tmp_holiday_loadData<-loadData[as.Date(loadData$LOAD_DATE)%in%as.Date(sim_date),]
	    	sim_load<-as.matrix(tmp_holiday_loadData[2:97])
	    	rownames(sim_load)<-tmp_holiday_loadData$LOAD_DATE
	    
	    	for(rname in rownames(sim_load)){
		    	if(as.Date(rname)==as.Date(tmp_pre_l_date)){
		    		sim_load[rname,]=sim_load[rname,]*lambda*as.numeric(date_and_coef[rname])
		    	}
		    	else{
		    		sim_load[rname,]=sim_load[rname,]*k*as.numeric(date_and_coef[rname])
		    	}
    		}
  
    		predictLoads[i,]<-apply(X = sim_load,MARGIN = 2,FUN = sum)
	}
  
  	LOAD_DATE<-strftime(data$h_weather_t_year$WETH_DATE,format='%Y-%m-%d')
  	predictLoads<-cbind(LOAD_DATE,as.data.frame(predictLoads))
  	return(predictLoads)
  }

holiday_dataSplit<-function(loadData,wethData,predate,ahead){
	#当前预测日期
	predate<-as.Date(predate)
	#去年同期
	same_day_l<-holiday_sameDayLyear(predate)
	#被预测的日期	
	h_date_t_year<-seq(predate, as.Date(ahead-1,origin =predate), 1)
	#被预测的日期去年同期	
	h_date_l_year<-seq(same_day_l, as.Date(ahead-1,origin =same_day_l), 1)
	#预测日期前60天中为周末的日期
	pr_h_date_t_year<-holiday_weekday(as.Date(-40,origin =predate),as.Date(-1,origin =predate))
	#预测日期去年同期前60天中为周末的日期
	pr_h_date_l_year<-holiday_weekday(as.Date(-40,origin =same_day_l),as.Date(-1,origin =same_day_l))
	  
	#被预测的日期负荷
	h_load_t_year<-loadData[as.Date(loadData$LOAD_DATE)%in%h_date_t_year,]
	#被预测的日期去年同期负荷
	h_load_l_year<-loadData[as.Date(loadData$LOAD_DATE)%in%h_date_l_year,]
	#预测日期前60天中为周末的日期负荷
	pr_h_load_t_year<-loadData[as.Date(loadData$LOAD_DATE)%in%pr_h_date_t_year,]
	#预测日期去年同期前60天中为周末的日期负荷
	pr_h_load_l_year<-loadData[as.Date(loadData$LOAD_DATE)%in%pr_h_date_l_year,]
	  
	#被预测的日期天气数据
	h_weather_t_year<-wethData[as.Date(wethData$WETH_DATE)%in%h_date_t_year,]
	#被预测的日期去年同期天气数据
	h_weather_l_year<-wethData[as.Date(wethData$WETH_DATE)%in%h_date_l_year,]
	#预测日期前60天中为周末的日期天气数据
	pr_h_weather_t_year<-wethData[as.Date(wethData$WETH_DATE)%in%pr_h_date_t_year,]
	#预测日期去年同期前60天中为周末的日期天气数据
	pr_h_weather_l_year<-wethData[as.Date(wethData$WETH_DATE)%in%pr_h_date_l_year,]
	  
	h_load_t_year<-holiday_loadDataOrder(h_load_t_year)
	h_load_l_year<-holiday_loadDataOrder(h_load_l_year)
	pr_h_load_t_year<-holiday_loadDataOrder(pr_h_load_t_year)
	pr_h_load_l_year<-holiday_loadDataOrder(pr_h_load_l_year)
	  
	h_weather_t_year<-holiday_wethDataOrder(h_weather_t_year)
	h_weather_l_year<-holiday_wethDataOrder(h_weather_l_year)
	pr_h_weather_t_year<-holiday_wethDataOrder(pr_h_weather_t_year)
	pr_h_weather_l_year<-holiday_wethDataOrder(pr_h_weather_l_year)
	  
	tmp_list<-list(h_load_t_year=h_load_t_year,h_load_l_year=h_load_l_year,pr_h_load_t_year=pr_h_load_t_year,
	 	pr_h_load_l_year=pr_h_load_l_year,h_weather_t_year=h_weather_t_year,
	 	h_weather_l_year=h_weather_l_year,pr_h_weather_t_year=pr_h_weather_t_year,
	 	pr_h_weather_l_year=pr_h_weather_l_year)
	return(tmp_list)
}

holiday_loadDataOrder<-function(holiday_loadData)
{
	return(holiday_loadData[order(holiday_loadData$LOAD_DATE),])
}

holiday_wethDataOrder<-function(weth_data)
{
	return(weth_data[order(weth_data$WETH_DATE),])
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
#计算综合系数
holiday_globalCoef<-function(temp_hist,temp_pre,date_hist,date_pre,n_top,alpha,beta_1,beta_2=.95) {
	temp_hist<-holiday_wethDataFormat(temp_hist)
	holiday_dateCorr<-holiday_dateCorr(date_hist,date_pre,beta_1,beta_2)
	holiday_tempCorr<-holiday_tempCorr(temp_hist,temp_pre,alpha)
	global<-holiday_dateCorr*holiday_tempCorr
	global<-global[order(global,decreasing = TRUE)]
	return(head(x = global,n = n_top))
}

holiday_wethDataFormat<-function(weather_f) {
	tmp<-as.matrix(weather_f[c('MIN_TMP','MAX_TMP')])
	rownames(tmp)<-strftime(weather_f$WETH_DATE,format='%Y-%m-%d')
	return(as.data.frame(t(tmp)))
}
holiday_loadData<-function(dframe,date) {
	tmp<-dframe[which(dframe$LOAD_DATE==date),]
	return(as.numeric(tmp[2:97]))
}
#计算往年节假日的系数
holiday_lambdaCoef<-function(loadData,pre_t_date,pre_l_date) {
	date_1<-seq(as.Date(-30,origin =pre_l_date), as.Date(-1,origin =pre_l_date),1)
	date_2<-seq(as.Date(-30,origin =pre_t_date),as.Date(-1,origin =pre_t_date), 1)
	data_1<-loadData[as.Date(loadData$LOAD_DATE)%in%date_1,]
	data_2<-loadData[as.Date(loadData$LOAD_DATE)%in%date_2,]
	mat_1<-as.matrix(data_1[2:97])
	mat_2<-as.matrix(data_2[2:97])
	sum_1<-apply(X = mat_1,MARGIN = 2,FUN = sum)/nrow(mat_1)
	sum_2<-apply(X = mat_2,MARGIN = 2,FUN = sum)/nrow(mat_2)
	return(as.numeric(sum_2/sum_1))
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
	return(tmpLyear$	date)
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

#获取预测日期中是节假日的日期
holiday_dateOfFestival<-function(startdate,ahead) {
	dates<-seq(as.Date(startdate), as.Date(ahead-1,startdate), by=1)
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
#生成指定区间内的日期序列
holiday_dateSeq<-function(f_date,t_date) {
	return(seq(as.Date(f_date), as.Date(t_date), by=1))
}

#日期按星期计算得分
holiday_dayTransWeekday<-function(vDate){
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
  weekeys=sort(unique(week))
  daynums=rep(0,length(vDate))
  daynums[which(week==weekeys[7])]=0.6
  daynums[which(week==weekeys[1])]=0.5
  daynums[which(week==weekeys[4])]=0.4
  daynums[which(week==weekeys[5])]=0.5
  daynums[which(week==weekeys[6])]=0.8
  daynums[which(week==weekeys[2])]=1.5
  daynums[which(week==weekeys[3])]=1.8
  return(daynums)
}

#节假日预测测试代码
test<-function(id) {

	preDate<-'2013-10-01'
	wethData<-weather_data(street = '西丽')
	loadData<-holiday_exeDm2015(paste0("select * from JL_TAIQU_LOAD where VKONT='",id,"';"))
	loadDataFilt<-holiday_loadDataFilter(loadData,k_size = 3)
  #     loadDataPredic<-holiday_prediction(0,loadData,wethData,'2013-10-01',ahead = 3)
  
  
  #     for(i in 1:nrow(realLoad)) {
  #       plot(as.numeric(realLoad[i,]),type = 'l',col='red')
  #       lines(as.numeric(loadDataPredic[i,]),type = 'l',col='blue')
  #     }
  
  ratio<-rep(0,100)
  for (i in 1:100) {
  	loadDataPredic<-holiday_prediction(0,loadDataFilt,wethData,'2013-10-01',ahead = 3,i/100,.2,.92)
  	ratio[i]<-holiday_errorStatic(loadData,loadDataPredic,preDate)
  }
}
holiday_errorStatic<- function (loadData,loadDataPredic,preDate) {
	dates<-seq(as.Date(preDate),as.Date(2,origin = preDate),1)
	tmp<-loadData[as.Date(loadData$LOAD_DATE)%in%dates,]
	realLoad<-tmp[2:97]
	rownames(realLoad)<-tmp$LOAD_DATE

	d_1<-as.matrix(abs(realLoad-loadDataPredic))/as.matrix(realLoad)
	t_1<-table(cut(d_1,breaks = c(0,.05,.1,.15,.2,.4,1)))/(96*3)
	return (sum (t_1[1:2]))
}


holiday_errorStatic<- function (loadData, loadDataPredic, preDate) {
	dates<-seq(as.Date(preDate), as.Date(2, origin = preDate), 1)
	tmp<-loadData[as.Date(loadData$LOAD_DATE)%in%dates, ]
	realLoad<-tmp[2:97]
	rownames(realLoad)<-tmp$LOAD_DATE

	d_1<-as.matrix(abs(realLoad-loadDataPredic))/as.matrix(realLoad)
	t_1<-table(cut(d_1, breaks = c(0, .05, .1, .15, .2, .4, 1)))/(96*3)
	return (sum (t_1[1:2]))
}

holiday_hitIndex <- function (mid, k_size){
	r<-floor(k_size/2)
	return(seq(mid-r,  mid+r,  1))
}

holiday_morphClose <- function (src, k_size){
	dst<-src
	r<-floor(k_size/2)
	dst[(r+1):(length(dst)-r)]<-sapply((r+1):(length(dst)-r), FUN=function(x){return(max(src[holiday_hitIndex(x, k_size)]))})
	dst[(r+1):(length(dst)-r)]<-sapply((r+1):(length(dst)-r), FUN=function(x){return(min(dst[holiday_hitIndex(x, k_size)]))})
	return(dst)
}

holiday_morphOpen <- function (src, k_size){
	dst<-src
	r<-floor(k_size/2)
	dst[(r+1):(length(dst)-r)]<-sapply((r+1):(length(dst)-r), FUN=function(x){return(min(src[holiday_hitIndex(x, k_size)]))})
	dst[(r+1):(length(dst)-r)]<-sapply((r+1):(length(dst)-r), FUN=function(x){return(max(dst[holiday_hitIndex(x, k_size)]))})
	return(dst)
}

holiday_loadDataFilter <- function (loadData, k_size){
	for (i in 1:nrow(loadData)) {
		tmp<-holiday_morphClose(as.numeric(loadData[i, ][2:97]), k_size)
		loadData[i, ][2:97]<-holiday_morphOpen(tmp, k_size)
	}
	return(loadData)
}

#通过最近邻方法对缺失值进行修正
holiday_SeqKNN<-function (data, k=10) {
	LOAD_DATE<-data$LOAD_DATE
	data<-data[-1]
	x <- as.matrix(data)
	N <- dim(x)
	p <- N[2]
	N <- N[1]
	nas <- is.na(drop(x %*% rep(1, p)))
	xcomplete <- x[!nas, ]
	xbad <- x[nas, , drop = FALSE]
	missing<-c()
  
  if(length(xbad)==0) {
    return(cbind(LOAD_DATE,data))
  }
  
	for (i in seq(nrow(xbad))) {
		missing[i]<-sum(is.na(xbad[i,]))
	}
	missingorder<-order(missing)

	xnas <- is.na(xbad)
	xbadhat <- xbad
	cat(nrow(xbad), fill = TRUE)
	for (i in seq(nrow(xbad))) {
		j<-order(missingorder[i])
		xinas <- xnas[missingorder[i], ]
		xbadhat[missingorder[i], ] <- holiday_nnmiss(xcomplete, xbad[missingorder[i], ], xinas, K = k)
		xcomplete<-rbind(xcomplete, xbadhat[missingorder[i],]) 
	}
	x[nas, ] <- xbadhat
	return(cbind(LOAD_DATE,data.frame(x)))
}
holiday_nnmiss<-function (x, xmiss, ismiss, K) 
{
	xd <- as.matrix(scale(x, xmiss, FALSE)[, !ismiss])
	dd <- drop(xd^2 %*% rep(1, ncol(xd)))
	od <- order(dd)[seq(K)]

	od<-od[!is.na(od)]
	K<-length(od)

	distance<-dd[od]
	s<-sum(1/(distance+0.000000000000001))
	weight<-(1/(distance+0.000000000000001))/s
  	xmiss[ismiss] <- drop(weight %*% x[od, ismiss, drop = FALSE]) ## weighted mean
  	##xmiss[ismiss] <- drop(rep(1/K, K) %*% x[od, ismiss, drop = FALSE])  ## mean
  	xmiss
}


holiday_plt<-function(preds, id) {

	tmp_dates<-as.character.factor(preds$LOAD_DATE)
	dates<-paste(tmp_dates,collapse="','")
	sql<-paste0("select * from JL_TAIQU_LOAD where VKONT ='",id,"' and LOAD_DATE in ('",dates,"') order by LOAD_DATE;")
	loadData<-holiday_exeDm2015(sql)
  	loadData<-loadData[5:100]
  
  	preds<-preds[-1]

	path<-paste0('~/Desktop/preds_',id)
  	if (!file.exists(path)){
    	dir.create(path)
  	}
  	setwd(path)
  	for(i in 1:nrow(preds)){
    	tiff(file=paste0('',tmp_dates[i],'.tiff'), res = 300, width = 2400, height = 2400, compression = "lzw")    
    	#画图
    	rang<-c(min(min(preds[i,]),min(loadData[i,])),max(max(preds[i,]),max(loadData[i,])))+c(-50,50)
    	
    	plot(as.numeric(preds[i,]),type='l',col = 'red',xaxt = 'n',ylim = rang,xlim = c(0,100),xlab = '时点',ylab = '负荷')
    	lines(as.numeric(loadData[i,]),type='l',col='blue')

    	legend("bottomright",legend=c('预测','原始'),col=c('red','blue'),lty=1)
    	dev.off()
  	} 
}

wethPlt<-function() {
	wethData<-read.csv("~/Desktop/福田区_竹子林基地.csv",header=T)
	wethData$WETH_DATE<-as.Date(wethData$WETH_DATE)
}
loadData=read.csv("~/Desktop/000047001611.csv",header=T)
loadData$X=NULL
loadData$LOAD_DATE=as.Date(loadData$LOAD_DATE)
loadData=loadData[order(loadData$LOAD_DATE),]
wethData=read.csv("~/Desktop/福田区_竹子林基地.csv",header=T)
wethData$X=NULL
wethData$WETH_DATE=as.Date(wethData$WETH_DATE)
holiday_init()
models=holiday_buildModel(loadData,wethData,startdate="2012-09-01",enddate="2014-08-20")
preds=holiday_prediction(models,loadData,wethData,predate="2014-9-2",ahead=7,alpha = .5)
holiday_plt(preds,id = '000047001766')
