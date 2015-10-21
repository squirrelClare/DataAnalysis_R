# 在数据库DM2015中执行sql，并返回查询的结果
methChoice_exeDm2015<-function(sql) {
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
methChoice_exeHumbird<-function(sql) {
	library(RMySQL)
	conn<-dbConnect(RMySQL::MySQL(),dbname='HUMMINGBIRD',username='humbird',host='192.168.10.87',
		port=3306,password='humbird123')
	res<-dbSendQuery(conn,statement=sql)
	mydata<-fetch(res,n=-1)
	dbDisconnect(conn)
	return(mydata)
}
#获取含有预测结果的表
methChoice_getPredTableName<-function() {
	sql<-"select table_name from information_schema.tables 
		where table_name like '%_preds';"
	tableNames<-unlist(methChoice_exeHumbird(sql))
	return(tableNames)
}
#获取各种方法的预测误差率结果
methChoice_getErrorRatio<-function(tableNames) {
	errorRatios<-list()
	for (tName in tableNames) {
		sql<-paste0("select * from ",tName,";")
		errorRatios[[tName]]<-methChoice_exeHumbird(sql)
	}
	return(errorRatios)
}
#最优算法提取
methChoice_bestMethod<-function(errorRatios) {
	#获取台区编号
	flag_id1s<-errorRatios[[1]]$flag_id1
	#获取表名，对应于不同的算法
	tableNames<-names(errorRatios)
	#台区个数
	nId<-length(flag_id1s)
	#表的个数
	nTable<-length(tableNames)
	#预测天数
	nPreDate<-ncol(errorRatios[[1]])-1
	
	#存储结果
	bestMeths<-matrix(NA,nrow = nId,ncol = nPreDate)
	colnames(bestMeths)<-colnames(errorRatios[[1]])[-1]
	
	#逐ID进行扫描
	for (i in 1:length(flag_id1s)) {
		id<-flag_id1s[i]
		# singleIdRatio<-matrix(NA,nrow = nTable,ncol = nPreDate)
		singleIdRatio<-data.frame()
		for (j in 1:length(tableNames)) {
			tName<-tableNames[j]

			tmpErrorRatio<-errorRatios[[tName]]
			singleIdRatio<-rbind(singleIdRatio,tmpErrorRatio[which(tmpErrorRatio$flag_id1==id),][-1])
		}
		#选取最好的算法，返回表的在tableNames中索引
		bestMeth<-apply(X =singleIdRatio,MARGIN =2,FUN = function(ratio) {
			return(which(ratio==min(ratio))[1])
			} )
		bestMeths[i,]<-as.numeric(bestMeth)
	}
	#转为数据框并且加入台区编号
	bestMeths<-as.data.frame.matrix(bestMeths)
	bestMeths<-cbind(flag_id1s,bestMeths)
	return(bestMeths)
}

#测试函数
methChoice_test<-function() {
	tableNames<-methChoice_getPredTableName()
	errorRatios<-methChoice_getErrorRatio(tableNames)
	bestMeths<-methChoice_bestMethod(errorRatios)

	library(RMySQL)
	conn<-dbConnect(RMySQL::MySQL(),dbname='DM2015',username='dimer',host='192.168.10.87',
		port=3306,password='data123')
	dbWriteTable(conn,name = 'best_method',bestMeths)
	dbDisconnect(conn)
}
methChoice_wethInf<-function(preDate,wethData) {
	wethData$WETH_DATE<-as.Date(wethData$WETH_DATE)

	dates<-seq(as.Date(-7,origin=preDate), as.Date(-1,origin=preDate), by=1)
	tmpWethData<-wethData[which(wethData$WETH_DATE%in%dates),]

	prevWethData<-wethData[which(wethData$WETH_DATE==as.Date(preDate)),]

	h_temp<-prevWethData$MAX_TMP
	l_temp<-prevWethData$MIN_TMP	
	type_week<-weekdays(as.Date(preDate))	
	month<-months(as.Date(preDate))
	h_temp_prev_week<-max(tmpWethData$MAX_TMP)	
	l_temp_prev_week<-min(tmpWethData$MIN_TMP)	
	h_temp_prev_week_var<-var(tmpWethData$MAX_TMP)	
	l_temp_prev_week_var<-var(tmpWethData$MIN_TMP)
	return(list(h_temp=h_temp,l_temp=l_temp,type_week=type_week,month=month,
							h_temp_prev_week=h_temp_prev_week,l_temp_prev_week=l_temp_prev_week,
							h_temp_prev_week_var=h_temp_prev_week_var,l_temp_prev_week_var=l_temp_prev_week_var))
}
