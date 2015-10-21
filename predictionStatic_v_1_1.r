##
#创建时间：2015-7-23
#功能：对已有8中模型的预测结果进行误差率统计，函数入口为predictionStatic_main，计算过程中排除掉了空值
##

# 在数据库DM2015中执行sql，并返回查询的结果
predictionStatic_exeLoadmonitoring <- function(sql) {
	library(RMySQL)
	conn <- dbConnect(RMySQL::MySQL(), dbname='loadmonitoring', username='develop', host='192.168.10.88', 
		port=3306, password='develop')
	dbSendQuery(conn, statement="set names 'utf8';")
	res <- dbSendQuery(conn, statement=sql)
	mydata <- fetch(res, n=-1)
	dbDisconnect(conn)
	return(mydata)
}

#获取预测负荷数据
predictionStatic_getPredictLoadData <- function(fDate, tDate, ids, preLoadTableName) {
	sql <- paste0("select * from ",preLoadTableName," where forecast_date >= '", fDate, "' and forecast_date <= '",
		tDate, "' and location_id in ('", paste0(ids, collapse = "','"), "') order by location_id, forecast_date;")
	predictLoadData <- predictionStatic_exeLoadmonitoring(sql)
	return(predictLoadData)
}

#获取真实负荷数据
predictionStatic_getRealLoadData <- function(fDate, tDate, ids, realLoadTableName) {
	sql <- paste0("select * from ", realLoadTableName, " where real_time >= '", fDate, "' and real_time <= '",
		tDate, "' and location_id in ('", paste0(ids, collapse = "','"), "') order by location_id, real_time;")
	realLoadData <- predictionStatic_exeLoadmonitoring(sql)
	return(realLoadData)
}

#统计每一种模型的预测误差率并保存到数据库
predictionStatic_staticSavePerModel <- function(realLoadTableName, preLoadTableName) {
	print(tName)
	print('START!!!')

	sql <- paste0("select distinct(forecast_date) from ",tName,";")
	forecastDate <- as.Date(unlist(predictionStatic_exeLoadmonitoring(sql)))   #获取真实表中日期
	fDate <-as.character(min(forecastDate))	#预测起始日
	tDate <-as.character(max(forecastDate))	#预测终止日
	

	sql <- paste0("select distinct(location_id) from ",tName,";")
	idsOfPreLoad <-unique(unlist(predictionStatic_exeLoadmonitoring(sql)))	#获取预测表中的id

	sql <- "select distinct(location_id) from loadmonitoring_load_realtime_201507;"
	idsOfSrcLoad <- unlist(predictionStatic_exeLoadmonitoring(sql))   #获取真实值表中的id

	idsOfIntersect <- intersect(idsOfSrcLoad, idsOfPreLoad)   #获取真实值和预测值表中的公共id
	
	nTimesThousand <- length(idsOfIntersect) %/% 1000

	errorRatioDataFrame <- predictionStatic_getMeanErroRatioOfPartId(fDate, tDate, idsOfIntersect[1:1000], 
		realLoadTableName, preLoadTableName)
	for (i in seq(nTimesThousand)[-1]) {
		fIndex <- (i-1)*1000 + 1
		tIndex <- i*1000
		ids <- idsOfIntersect[fIndex : tIndex]
		errorRatioDataFrame <- rbind(errorRatioDataFrame, 
			predictionStatic_getMeanErroRatioOfPartId(fDate, tDate, ids, realLoadTableName, preLoadTableName))
	}

	ids <- idsOfIntersect[(nTimesThousand * 1000 + 1) + length(idsOfIntersect)]
	errorRatioDataFrame <- rbind(errorRatioDataFrame, 
		predictionStatic_getMeandsaErroRatioOfPartId(fDate, tDate, ids, realLoadTableName, preLoadTableName))

	print('SUCCESS!!!')
}

#批量计算专变或者台区的平均误差率
predictionStatic_getMeanErroRatioOfPartId <- function(fDate, tDate, ids, realLoadTableName, preLoadTableName) {
	srcLoadData <- predictionStatic_getRealLoadData(fDate, tDate, ids, realLoadTableName)	#获取真实负荷值
	preLoadData <- predictionStatic_getPredictLoadData(fDate, tDate, ids, realLoadTableName)	#获取预测负荷值

	ratioDataFrame <- predictionStatic_getMeanErroRatioOfPerId(srcLoadData, preLoadData, fDate, tDate, ids[1])

	for (id in ids[-1]) {
		ratioDataFrame <- rbind(predictionStatic_getMeanErroRatioOfPerId(srcLoadData, preLoadData, fDate, tDate, id))
	}
	return(ratioDataFrame)
} 

#计算单个专变或者台区的平均误差率
predictionStatic_getMeanErroRatioOfPerId <- function(srcLoadData, preLoadData, fDate, tDate, id) {
	nDay <- as.numeric(as.Date(tDate) - as.Date(fDate)) + 1

	src <- srcLoadData[which(srcLoadData$location_id == id), ]	#读取真实负荷表中的数据
	src <- src[order(src$real_time), ]
	pre <- preLoadData[which(preLoadData$location_id == id), ]	#读取预测负荷表中的数据
	pre <- pre[order(pre$forecast_date), ]
	if (nrow(src) != nDay | nrow(pre) != nDay) {
	  	return(NULL);
	} 
 	src <- src[2:97]	#提取负荷数据
	pre <- pre[2:97]	#提取负荷数据 	

  	#误差率计算
	ratioData <- sapply(X = seq(ncol(pre)), FUN=function(x) {
		errorRatio <- abs(pre[[x]]-src[[x]])/src[[x]]	
		errorRatio[which(errorRatio == Inf)] <- NA
		return(errorRatio)
	})
	meanRatioData <- round(apply(X = ratioData, MARGIN = 1, FUN = mean, na.rm=T),digits = 4)		#平均误差率计算
	
	#结果整合为一个数据框
	ratioIdDate <- data.frame(location_id = rep(id, nDay), forecast_date = seq(as.Date(fDate), as.Date(tDate), 1), 
		meanRatio = meanRatioData)
	return(ratioIdDate)
}

#获取含有预测结果的表
predictionStatic_getPredTableName <- function(partName) {
	sql <- paste0("select table_name from information_schema.tables 
		where table_name like '%",partName,"';")
	tableNames <- unlist(predictionStatic_exeHumbird(sql))
	names(tableNames) <- NULL
	return(tableNames)
}
predictionStatic_main <- function(partName) {
	tNames <- predictionStatic_getPredTableName(partName)
	for (tName in tNames) {
		predictionStatic_staticSavePerModel(tName)
	}
}
