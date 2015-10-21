##
#创建时间：2015-7-7
#功能：对已有8中模型的预测结果进行误差率统计，函数入口为predictionStatic_main
##


#在数据库HUMBIRD中执行sql，并返回查询的结果
predictionStatic_exeHumbird <- function(sql) {
	library(RMySQL)
	
	conn <- dbConnect(RMySQL::MySQL(), dbname='HUMMINGBIRD', username='humbird', host='192.168.10.87', 
		port=3306, password='humbird123')
	res <- dbSendQuery(conn, statement=sql)
	mydata <- fetch(res, n=-1)
	dbDisconnect(conn)
	return(mydata)
}

# 在数据库DM2015中执行sql，并返回查询的结果
predictionStatic_exeDm2015 <- function(sql) {
	library(RMySQL)
	conn <- dbConnect(RMySQL::MySQL(), dbname='DM2015', username='dimer', host='192.168.10.87', 
		port=3306, password='data123')
	dbSendQuery(conn, statement="set names 'utf8';")
	res <- dbSendQuery(conn, statement=sql)
	mydata <- fetch(res, n=-1)
	dbDisconnect(conn)
	return(mydata)
}

#获取预测负荷数据
predictionStatic_getPredictLoadData <- function(tName) {
	sql <- paste0("select * from ", tName, " order by VID, LOAD_DATE;")
	predictLoadData <- predictionStatic_exeHumbird(sql)
	return(predictLoadData)
}

#获取真实负荷数据
predictionStatic_getRealLoadData <- function(fDate, tDate, ids) {
	sql <- paste0("select * from JL_TAIQU_LOAD where LOAD_DATE >= '", fDate, "' and LOAD_DATE <= '",
		tDate, "' and VKONT in ('", paste0(ids, collapse = "','"), "') order by VKONT, LOAD_DATE;")
	realLoadData <- predictionStatic_exeDm2015(sql)
	return(realLoadData)
}

#统计每一种模型的预测误差率并保存到数据库
predictionStatic_staticSavePerModel <- function(tName) {
	print(tName)
	print('START!!!')

	preLoadData <- predictionStatic_getPredictLoadData(tName)	#读取预测负荷表中的数据
	tmpLoadDate <- as.Date(preLoadData$LOAD_DATE)
	fDate <-as.character(min(tmpLoadDate))	#预测起始日
	tDate <-as.character(max(tmpLoadDate))	#预测终止日
	ids <-preLoadData$VID
	srcLoadData <- predictionStatic_getRealLoadData(fDate, tDate, ids)	#读取真实负荷表中的数据

	preLoadData <- preLoadData[2:97]	#提取负荷数据
	srcLoadData <- srcLoadData[5:100]	#提取负荷数据

	#误差率计算
	ratioData <- sapply(X = seq(ncol(preLoadData)), FUN=function(x) {
		errorRatio <- abs(preLoadData[[x]]-srcLoadData[[x]])/srcLoadData[[x]]	
		errorRatio[which(errorRatio == Inf)] <- NA
		return(errorRatio)
		})
	meanRatioData <- round(apply(X = ratioData, MARGIN = 1, FUN = mean),digits = 4)		#平均误差率计算
	
	#结果整合为一个数据框
	ratioIdDate <- data.frame(id = ids, LOAD_DATE = tmpLoadDate, meanRatio = meanRatioData)
	dstTName <- paste0(tName,'_preds')

	#整合结果写入数据库
	library(RMySQL)
	conn <- dbConnect(RMySQL::MySQL(), dbname='DM2015', username='dimer', host='192.168.10.87', 
									port=3306, password='data123')
	dbWriteTable(conn, name = dstTName, ratioIdDate)
	dbDisconnect(conn)

	print('SUCCESS!!!')
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