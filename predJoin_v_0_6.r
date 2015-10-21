##
#创建日期：2015-07-07
#功能：整合已统计出的误差率数据
##

# 在数据库DM2015中执行sql，并返回查询的结果
methChoice_exeDm2015 <- function(sql) {
	library(RMySQL)
	conn <- dbConnect(RMySQL::MySQL(), dbname='DM2015', username='dimer', host='192.168.10.87',
									port=3306, password='data123')
	dbSendQuery(conn, statement="set names 'utf8';")
	res <- dbSendQuery(conn, statement=sql)
	mydata <- fetch(res, n=-1)
	dbDisconnect(conn)
	return(mydata)
}

#在数据库HUMBIRD中执行sql，并返回查询的结果
methChoice_exeHumbird <- function(sql) {
	library(RMySQL)
	conn <- dbConnect(RMySQL::MySQL(), dbname='HUMMINGBIRD', username='humbird', host='192.168.10.87',
									port=3306, password='humbird123')
	res <- dbSendQuery(conn, statement=sql)
	mydata <- fetch(res, n=-1)
	dbDisconnect(conn)
	return(mydata)
}
#获取含有预测结果的表
methChoice_getPredTableName <- function(partName) {
	sql <- paste0("select table_name from information_schema.tables
		where table_name like '%", partName, "%';")
	tableNames <- unlist(methChoice_exeDm2015(sql))

	return(unname(tableNames))
}
#获取各种方法的预测误差率结果
methChoice_getErrorRatio <- function(tableNames) {
	sql <- paste0("select * from ", tableNames[1], " order by id,LOAD_DATE;")
	tmpRatio <- methChoice_exeDm2015(sql)
	ratioFrame <- data.frame(id = tmpRatio$id, LOAD_DATE = tmpRatio$LOAD_DATE)

	tNameSplit <- function(tName) {
		tmp <- strsplit(tName,split = '')[[1]]
		tmp <-tmp[-length(tmp):(-length(tmp)+13)]
		return(paste0(tmp,collapse = ''))
	}

	for (tName in tableNames) {
		sql <- paste0("select meanRatio from ", tName, " order by id,LOAD_DATE;")
		tmpRatio <- methChoice_exeDm2015(sql)
		tmpName <- tNameSplit(tName)
		print(tName)
		print(nrow(tmpRatio))
		ratioFrame[[tmpName]] <- tmpRatio$meanRatio
	}
	return(na.omit(ratioFrame))
}
#最优算法提取
methChoice_bestMethod <- function(errorRatios) {
	id <- errorRatios$id
	loadDate <- errorRatios$LOAD_DATE
	errorRatios$id <-NULL
	errorRatios$LOAD_DATE <- NULL
	errorRatios <- as.matrix.data.frame(errorRatios)

	methodNames <- colnames(errorRatios)
	bestMethods <- apply(X = errorRatios, MARGIN = 1,FUN = function(e) {
		return(methodNames[which.min(e)])
	})

	dstBestMethods <- data.frame(id = id, LOAD_DATE = loadDate, bMethod = unname(bestMethods))
	return(dstBestMethods)
}

#测试函数()建立台区编号、预测日期、最优算法对应表
methChoice_methodTableBuildSave <- function(partName,dstTableName) {
	#整合八张表
	tableNames <- methChoice_getPredTableName(partName)
	errorRatios <- methChoice_getErrorRatio(tableNames)
	bestMeths <- methChoice_bestMethod(errorRatios)

	#整合结果写入数据库
	library(RMySQL)
	conn <- dbConnect(RMySQL::MySQL(), dbname='DM2015', username='dimer', host='192.168.10.87',
									port=3306, password='data123')
	dbWriteTable(conn, name = dstTableName, bestMeths)
	dbDisconnect(conn)

	#生成特征表，包含气温负荷特征和最佳算法
	# methChoice_generatTable()
}

#获取与天气星期相关的参数
methChoice_wethInf <- function(preDate, wethData) {
	#基础天气数据信息
	baseWethInf <- methChoice_baseWethInf(preDate, wethData)
	#预测日前5个同类型日气温信息
	sameDayWethInf <- methChoice_sameDayWethInf(preDate, wethData)
	return(append(baseWethInf, sameDayWethInf))
}

#获取与负荷相关的参数
methChoice_loadInf <- function(preDate, loadData, ahead) {
	#基础负荷信息
	baseLoadInf <- methChoice_baseLoadInf(as.character(as.Date(preDate)-ahead+1), loadData)
	#预测日前五个同类型日负荷信息
	sameDayLoadInf <- methChoice_sameDayLoadInf(preDate, loadData)
	return(append(baseLoadInf, sameDayLoadInf))
}

methChoice_generatTable <- function() {
	#编号最有方法表
	bestMeths <- methChoice_exeDm2015('select * from best_method;')
	bestMeths$row_names <- NULL
	#预测日期
	preDate <- '2014-9-1'
	#预测天数
	ahead <- 7
	#天气数据
	sqlWethData <- "select * from L1_WEATHER_HISTORY;"
	wethData <- methChoice_exeHumbird(sqlWethData)
	#初始化最终结果存储数据框
	result <- array()
	for (i in 1:nrow(bestMeths)) {
		#单个台区最优方法
		record <- unlist(bestMeths[i, ])
		#台区编号
		vkont <- record[1]

		#获取台区负荷数据
		sqlLoadData <- paste0("select * from JL_TAIQU_LOAD where VKONT='", vkont, "' order
			by LOAD_DATE;")
		loadData <- methChoice_exeDm2015(sqlLoadData)[4:100]
		for (j in 1:7) {

			#气温参数
			wethInf <- methChoice_wethInf(as.character(as.Date(preDate)+j-1), wethData)
			loadInf <- methChoice_loadInf(as.character(as.Date(preDate)+j-1), loadData, j)
			# tmp <- c(id=vkont)
			tmp <- c(vkont)
			tmp <- append(tmp, wethInf)
			tmp <- append(tmp, loadInf)
			tmp <- append(tmp, c(method=record[j+1]))
			# 			tmp <- t(as.data.frame.vector(tmp))
			# 			rownames(tmp) <- NULL
			result <- rbind(result, tmp)
		}
		print(i)
	}
	result <- as.data.frame.array(result)
	result <- na.omit(result)
	tableNames <- methChoice_getPredTableName()
	row.names(result) <- NULL
	result$method.rates_s_1 <- tableNames[result$method.rates_s_1]

	return(result)
}

#获取指定日期前五个同类型日
methChoice_sameDay <- function(preDate) {
	dates <- seq(as.Date(-38, origin=preDate),  as.Date(-1, origin = preDate),  by=1)
	return(dates[which(weekdays(dates)==weekdays(as.Date(preDate)))])
}

#预测日前5个同类型日气温信息整合
methChoice_sameDayWethInf <- function(preDate, wethData) {
	#天气数据预处理
	wethData$WETH_DATE <- as.Date(wethData$WETH_DATE)
	#前五个同类型日日期
	sameDates <- methChoice_sameDay(preDate)
	#前五个同类型日天气数据
	sameDatesWethData <- wethData[which(wethData$WETH_DATE%in%sameDates), ]
	# 预测日前5个同类型日最高气温
	h_temp_same_day <- max(sameDatesWethData$MAX_TMP)
	# 预测日前5个同类型日最低气温
	l_temp_same_day <- max(sameDatesWethData$MIN_TMP)
	# 预测日前5个同类型日平均气温
	m_temp_same_day <- mean(append(wethData$MAX_TMP, wethData$MIN_TMP))
	# 预测日前5个同类型日最高气温标准差
	h_temp_same_day_sd <- sd(sameDatesWethData$MAX_TMP)
	# 预测日前5个同类型日最低气温标准差
	l_temp_same_day_sd <- sd(sameDatesWethData$MIN_TMP)
	# 预测日前5个同类型日平均气温标准差
	m_temp_same_day_sd <- sd(apply(X=cbind(sameDatesWethData$MAX_TMP,
		sameDatesWethData$MIN_TMP), MARGIN=1, FUN=mean))
	#预测日前5个同类型日气温信息整合
	sameDatesWethInf <- c(h_temp_same_day=h_temp_same_day, l_temp_same_day=l_temp_same_day,
		m_temp_same_day=m_temp_same_day, h_temp_same_day_sd=h_temp_same_day_sd,
		l_temp_same_day_sd=l_temp_same_day_sd, m_temp_same_day_sd=m_temp_same_day_sd)
	return(sameDatesWethInf)
}
#基本气温数据信息
methChoice_baseWethInf <- function(preDate, wethData) {
	#天气数据预处理
	wethData$WETH_DATE <- as.Date(wethData$WETH_DATE)

	#预测日前一周日期
	dates <- seq(as.Date(-7, origin=preDate),  as.Date(-1, origin=preDate),  by=1)
	#预测日前一周气温数据
	tmpWethData <- wethData[which(wethData$WETH_DATE%in%dates), ]
	#预测日当天气温数据
	prevWethData <- wethData[which(wethData$WETH_DATE==as.Date(preDate)), ]

	#预测日最高气温
	h_temp <- prevWethData$MAX_TMP
	#预测日最低气温
	l_temp <- prevWethData$MIN_TMP
	#预测日的星期类型
	type_week <- weekdays(as.Date(preDate))
	#预测日所在月份
	month <- months(as.Date(preDate))
	#预测日前一周最高气温
	h_temp_prev_week <- max(tmpWethData$MAX_TMP)
	#预测日前一周最低气温
	l_temp_prev_week <- min(tmpWethData$MIN_TMP)
	#预测日前一周最高气温方差（每天最高气温）
	h_temp_prev_week_sd <- sd(tmpWethData$MAX_TMP)
	#预测日前一周最低气温方差（每天最低气温）
	l_temp_prev_week_sd <- sd(tmpWethData$MIN_TMP)

	return(c(h_temp=h_temp, l_temp=l_temp, type_week=type_week, month=month,
					 h_temp_prev_week=h_temp_prev_week, l_temp_prev_week=l_temp_prev_week,
					 h_temp_prev_week_sd=h_temp_prev_week_sd, l_temp_prev_week_sd=l_temp_prev_week_sd))
}

#基本负荷信息
methChoice_baseLoadInf <- function(preDate, loadData) {
	loadData$LOAD_DATE <- as.Date(loadData$LOAD_DATE)

	dates <- seq(as.Date(-7, origin=preDate),  as.Date(-1, origin=preDate),  by=1)
	tmpLoadData <- as.matrix.data.frame(loadData[which(loadData$LOAD_DATE%in%dates), ][-1])

	#预测日前一周最高负荷
	h_load_prev_week <- max(tmpLoadData)
	#预测日前一周最低负荷
	l_load_prev_week <- min(tmpLoadData)
	#预测日前一周等单天最高负高荷的标准差
	h_load_prev_week_sd <- sd(apply(tmpLoadData, MARGIN = 1, FUN = max))
	#预测日前一周等单天最高低负荷的标准差
	l_load_prev_week_sd <- sd(apply(tmpLoadData, MARGIN = 1, FUN = min))
	return(c(h_load_prev_week=h_load_prev_week, l_load_prev_week=l_load_prev_week,
					 h_load_prev_week_sd=h_load_prev_week_sd, l_load_prev_week_sd=l_load_prev_week_sd))
}

#预测日前5个同类型日气温信息整合
methChoice_sameDayLoadInf <- function(preDate, loadData) {
	#前五个同类型日日期
	sameDates <- methChoice_sameDay(preDate)
	#负荷数据日期预处理
	loadData$LOAD_DATE <- as.Date(loadData$LOAD_DATE)
	#前五个同类型日负荷数据
	sameDatesLoadData <- as.matrix.data.frame(loadData[which(loadData$LOAD_DATE%in%sameDates), ][-1])
	# 预测日前5个同类型日最高负荷
	h_load_same_day <- max(sameDatesLoadData)
	# 预测日前5个同类型日最低负荷
	l_load_same_day <- min(sameDatesLoadData)
	# 预测日前5个同类型日平均负荷
	m_load_same_day <- mean(sameDatesLoadData)
	# 预测日前5个同类型日最高负荷标准差
	h_load_same_day_sd <- sd(apply(sameDatesLoadData, MARGIN = 1, FUN = max))
	# 预测日前5个同类型日最低负荷标准差
	l_load_same_day_sd <- sd(apply(sameDatesLoadData, MARGIN = 1, FUN = min))
	# 预测日前5个同类型日平均负荷标准差
	m_load_same_day_sd <- sd(apply(sameDatesLoadData, MARGIN = 1, FUN = mean))

	loadInf <- c(h_load_same_day=h_load_same_day, l_load_same_day=l_load_same_day,
		m_load_same_day=m_load_same_day, h_load_same_day_sd=h_load_same_day_sd,
		l_load_same_day_sd=l_load_same_day_sd, m_load_same_day_sd=m_load_same_day_sd)
	return(loadInf)
}
