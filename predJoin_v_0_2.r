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
	tableNames<-unlist(methChoice_exeDm2015(sql))
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

#测试函数()建立台区编号、预测日期、最优算法对应表
methChoice_test<-function() {
	tableNames<-methChoice_getPredTableName()
	errorRatios<-methChoice_getErrorRatio(tableNames)
	bestMeths<-methChoice_bestMethod(errorRatios)
	
	library(RMySQL)
	conn<-dbConnect(RMySQL::MySQL(),dbname='DM2015',username='dimer',host='192.168.10.87',
									port=3306,password='data123')
	ddbSendQuery("set names 'utf8';")
	dbWriteTable(conn,name = 'best_method',bestMeths)
	dbDisconnect(conn)
	
	
	library(RMySQL)
	conn<-dbConnect(RMySQL::MySQL(),dbname='DM2015',username='dimer',host='192.168.10.87',
									port=3306,password='data123')
	dbWriteTable(conn,name = 'method_choice_2',mydata)
	dbDisconnect(conn)
}

#获取与天气星期相关的参数
methChoice_wethInf<-function(preDate,wethData) {
	wethData$WETH_DATE<-as.Date(wethData$WETH_DATE)
	
	dates<-seq(as.Date(-7,origin=preDate), as.Date(-1,origin=preDate), by=1)
	tmpWethData<-wethData[which(wethData$WETH_DATE%in%dates),]
	
	prevWethData<-wethData[which(wethData$WETH_DATE==as.Date(preDate)),]
	
	#预测日最高气温
	h_temp<-prevWethData$MAX_TMP
	#预测日最低气温
	l_temp<-prevWethData$MIN_TMP
	#预测日的星期类型	
	type_week<-weekdays(as.Date(preDate))
	#预测日所在月份	
	month<-months(as.Date(preDate))
	#预测日前一周最高气温
	h_temp_prev_week<-max(tmpWethData$MAX_TMP)
	#预测日前一周最低气温	
	l_temp_prev_week<-min(tmpWethData$MIN_TMP)
	#预测日前一周最高气温方差（每天最高气温）	
	h_temp_prev_week_sd<-sd(tmpWethData$MAX_TMP)
	#预测日前一周最低气温方差（每天最低气温）	
	l_temp_prev_week_sd<-sd(tmpWethData$MIN_TMP)
	return(c(h_temp=h_temp,l_temp=l_temp,type_week=type_week,month=month,
					 h_temp_prev_week=h_temp_prev_week,l_temp_prev_week=l_temp_prev_week,
					 h_temp_prev_week_sd=h_temp_prev_week_sd,l_temp_prev_week_sd=l_temp_prev_week_sd))
}

#获取与负荷相关的参数
methChoice_loadInf<-function(preDate,loadData) {
	loadData$LOAD_DATE<-as.Date(loadData$LOAD_DATE)
	
	dates<-seq(as.Date(-7,origin=preDate), as.Date(-1,origin=preDate), by=1)
	tmpLoadData<-as.matrix.data.frame(loadData[which(loadData$LOAD_DATE%in%dates),][-1])
	
	#预测日前一周最高负荷
	h_load_prev_week<-max(tmpLoadData)
	#预测日前一周最低负荷
	l_load_prev_week<-min(tmpLoadData)
	#预测日前一周等单天最高负高荷的标准差
	h_load_prev_week_sd<-sd(apply(tmpLoadData,MARGIN = 1,FUN = max))
	#预测日前一周等单天最高低负荷的标准差
	l_load_prev_week_sd<-sd(apply(tmpLoadData,MARGIN = 1,FUN = min))
	return(c(h_load_prev_week=h_load_prev_week,l_load_prev_week=l_load_prev_week,
					 h_load_prev_week_sd=h_load_prev_week_sd,l_load_prev_week_sd=l_load_prev_week_sd))
}

methChoice_generatTable<-function() {
	#编号最有方法表
	bestMeths<-methChoice_exeDm2015('select * from best_method;')
	bestMeths$row_names<-NULL
	#预测日期
	preDate<-'2014-9-1'
	#预测天数
	ahead<-7
	#天气数据
	sqlWethData<-"select * from L1_WEATHER_HISTORY;"
	wethData<-methChoice_exeHumbird(sqlWethData)
	#初始化最终结果存储数据框
	result<-array()
	for (i in 1:nrow(bestMeths)) {
		#单个台区最优方法
		record<-unlist(bestMeths[i,])
		#台区编号
		vkont<-record[1]
		
		#获取台区负荷数据
		sqlLoadData<-paste0("select * from JL_TAIQU_LOAD where VKONT='",vkont,"' order
			by LOAD_DATE;")
		loadData<-methChoice_exeDm2015(sqlLoadData)[4:100]
		for (j in 1:7) {
			
			#气温参数
			wethInf<-methChoice_wethInf(as.character(as.Date(preDate)+j-1),wethData)
			loadInf<-methChoice_loadInf(preDate,loadData)
			# tmp<-c(id=vkont)
			tmp<-c(vkont)
			tmp<-append(tmp,wethInf)
			tmp<-append(tmp,loadInf)
			tmp<-append(tmp,c(method=record[j+1]))
			# 			tmp<-t(as.data.frame.vector(tmp))
			# 			rownames(tmp)<-NULL
			result<-rbind(result,tmp)
		}
		print(i)
	}
	result<-as.data.frame.array(result)
	result<-na.omit(result)
	row.names(result)<-NULL
	
	# result$h_temp<-as.numeric(result$h_temp)
	# result$l_temp<-as.numeric(result$l_temp)
	
	# result$h_temp_prev_week<-as.numeric(result$h_temp_prev_week)
	# result$h_temp_prev_week_sd<-as.numeric(result$h_temp_prev_week_sd)
	
	# result$l_temp_prev_week<-as.numeric(result$l_temp_prev_week)
	# result$l_temp_prev_week_sd<-as.numeric(result$l_temp_prev_week_sd)
	
	# result$h_load_prev_week<-as.numeric(result$h_load_prev_week)
	# result$h_load_prev_week_sd<-as.numeric(result$h_load_prev_week_sd)
	
	# result$l_load_prev_week<-as.numeric(result$l_load_prev_week)
	# result$l_load_prev_week_sd<-as.numeric(result$l_load_prev_week_sd)
	
	# result$method.rates_s_1<-as.numeric(result$method.rates_s_1)
	return(result)
}