##
#创建时间：2015-7-8
#功能：计算已有8中模型对单个台区单天预测成功概率，成功定义为平均误差率低于10%
##

# 在数据库DM2015中执行sql，并返回查询的结果
logisticMethChoice_exeDm2015 <- function(sql) {
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
logisticMethChoice_exeHumbird <- function(sql) {
	library(RMySQL)
	conn <- dbConnect(RMySQL::MySQL(), dbname='HUMMINGBIRD', username='humbird', host='192.168.10.87', 
									port=3306, password='humbird123')
	res <- dbSendQuery(conn, statement=sql)
	mydata <- fetch(res, n=-1)
	dbDisconnect(conn)
	return(mydata)
}

#获取与天气星期相关的参数
logisticMethChoice_wethInf <- function(preDate, wethData,ahead) {
	#基础天气数据信息
	baseWethInf <- logisticMethChoice_baseWethInf(preDate, wethData)
	#预测日前5个同类型日气温信息
	sameDayWethInf <- logisticMethChoice_sameDayWethInf(preDate, wethData)
	#前一周同天气温信息
	preSameDayWethInf <- logisticMethChoice_prevSameDayWethInf(preDate, wethData)
	#最相近日气温信息
	similarDayWethInf <- logisticMethChoice_similarDayWethInf(preDate, wethData, ahead)
	return(c(baseWethInf, sameDayWethInf, preSameDayWethInf, similarDayWethInf))
}

#获取与负荷相关的参数
logisticMethChoice_loadInf <- function(preDate, loadData, wethData, ahead) {
	#基础负荷信息
	baseLoadInf <- logisticMethChoice_baseLoadInf(as.character(as.Date(preDate)-ahead+1), loadData)
	#预测日前五个同类型日负荷信息
	sameDayLoadInf <- logisticMethChoice_sameDayLoadInf(preDate, loadData)
	#前一周同天负荷信息
	preSameDayLoadInf <- logisticMethChoice_prevSameDayLoadInf(preDate, loadData)
	#最相近日气温信息
	similarDayLoadInf <- logisticMethChoice_similarDayLoadInf(preDate, loadData,wethData,ahead)
	res <- c(baseLoadInf, sameDayLoadInf, preSameDayLoadInf, similarDayLoadInf)
	return(res)
}
logisticMethChoice_generatTable <- function(tableName) {
	ratioData <- logisticMethChoice_exeDm2015(paste0('select * from ',tableName,' ;'))
	ratioData$row_names <- NULL
	#预测日期
	preDate <- as.character(min(as.Date(ratioData$LOAD_DATE)))
	#预测天数
	ahead <- 7
	#天气数据
	sqlWethData <- "select * from L1_WEATHER_HISTORY;"
	wethData <- logisticMethChoice_exeHumbird(sqlWethData)


	#初始化最终结果存储数据框
	result <-NULL
	for (vkont in unique(ratioData$id)) {		
		#获取台区负荷数据
		sqlLoadData <- paste0("select * from JL_TAIQU_LOAD where VKONT='", vkont, "' order
			by LOAD_DATE;")
		loadData <- logisticMethChoice_exeDm2015(sqlLoadData)[4:100]
		for (j in 1:7) {
			tmpDate <- as.character(as.Date(preDate)+j-1)
			#气温参数

			meanRatio <- ratioData[which(ratioData$id == vkont & ratioData$LOAD_DATE == tmpDate), ]$meanRatio

			tmp <- NULL
			tmp <- c(vkont = vkont,LOAD_DATE = tmpDate)
			tmp <- append(tmp, wethInf)
			tmp <- append(tmp, loadInf)
			tmp <- append(tmp, c(success = ifelse(meanRatio <= 0.1, 'yes', 'no')))
			if (is.null(result)) {
				result <-array(tmp)
			}else{
				result <- rbind(result, tmp)	
			}	
		}
		print(vkont)
	}
	result <- as.data.frame.array(result)
	# result$success <- ifelse(ratioData$meanRatio <= 0.1, 'yes', 'no')
	row.names(result) <- NULL
	result <- na.omit(result)
	write.csv(x = result,file = paste0('E:/',tableName,'_new.csv'))		#写入本地
	return(result)
}

#获取指定日期前五个同类型日
logisticMethChoice_sameDay <- function(preDate) {
	dates <- seq(as.Date(-38, origin=preDate),  as.Date(-1, origin = preDate),  by=1)
	return(dates[which(weekdays(dates)==weekdays(as.Date(preDate)))])
}

#预测日前5个同类型日气温信息整合
logisticMethChoice_sameDayWethInf <- function(preDate, wethData) {
	#天气数据预处理
	wethData$WETH_DATE <- as.Date(wethData$WETH_DATE)
	#前五个同类型日日期
	sameDates <- logisticMethChoice_sameDay(preDate)
	#前五个同类型日天气数据
	sameDatesWethData <- wethData[which(wethData$WETH_DATE%in%sameDates), ]

	if (nrow(sameDatesWethData) != 0) {
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
	}else {
		sameDatesWethInf <- c(h_temp_same_day = NA, l_temp_same_day = NA, m_temp_same_day = NA,
		 	h_temp_same_day_sd = NA, l_temp_same_day_sd = NA, m_temp_same_day_sd = NA)
	}
	return(sameDatesWethInf)
}

#前一周同一天温度信息
logisticMethChoice_prevSameDayWethInf <- function(preDate, wethData) {
	#天气数据预处理
	wethData$WETH_DATE <- as.Date(wethData$WETH_DATE)
	#前一周同一天气温数据
	tmpWethData <- wethData[which(wethData$WETH_DATE == as.character(as.Date(-7,origin = preDate))),]
	if (nrow(tmpWethData) != 0) {
		#前一周同一天最高气温
		h_temp_prev_sameday <- tmpWethData$MAX_TMP
		#前一周同一天最低气温
		l_temp_prev_sameday <- tmpWethData$MIN_TMP
	}else {
		h_temp_prev_sameday <- NA
		l_temp_prev_sameday <- NA
	}
	return(c(h_temp_prev_sameday = h_temp_prev_sameday, l_temp_prev_sameday = l_temp_prev_sameday))
}
#前一个月最相近日的气温信息
logisticMethChoice_similarDayWethInf <- function(preDate, wethData, ahead) {
	similarDate <- logisticMethChoice_similarDay(preDate, wethData, ahead)
	tmpWethData <- wethData[wethData$WETH_DATE == similarDate, ]
	if (nrow(tmpWethData) != 0) {
		#最高气温
		h_temp_similarday <- tmpWethData$MAX_TMP
		#最低气温
		l_tem_similarday <- tmpWethData$MIN_TMP
	}else {
		h_temp_similarday <- NA
		l_tem_similarday <- NA
	}

	return(c(h_temp_similarday = h_temp_similarday, l_tem_similarday = l_tem_similarday))
}
#基本气温数据信息
logisticMethChoice_baseWethInf <- function(preDate, wethData) {
	#天气数据预处理
	wethData$WETH_DATE <- as.Date(wethData$WETH_DATE)
	
	#预测日前一周日期
	dates <- seq(as.Date(-7, origin=preDate),  as.Date(-1, origin=preDate),  by=1)
	#预测日前一周气温数据
	tmpWethData <- wethData[which(wethData$WETH_DATE %in% dates), ]
	#预测日当天气温数据
	prevWethData <- wethData[which(wethData$WETH_DATE == as.Date(preDate)), ]
	
	if (nrow(tmpWethData) != 0) {
		h_temp_prev_week <- max(tmpWethData$MAX_TMP)
		#预测日前一周最低气温	
		l_temp_prev_week <- min(tmpWethData$MIN_TMP)
		#预测日前一周最高气温方差（每天最高气温）	
		h_temp_prev_week_sd <- sd(tmpWethData$MAX_TMP)
		#预测日前一周最低气温方差（每天最低气温）	
		l_temp_prev_week_sd <- sd(tmpWethData$MIN_TMP)
	}else {
		h_temp_prev_week <- NA
		#预测日前一周最低气温	
		l_temp_prev_week <- NA
		#预测日前一周最高气温方差（每天最高气温）	
		h_temp_prev_week_sd <- NA
		#预测日前一周最低气温方差（每天最低气温）	
		l_temp_prev_week_sd <- NA
	}

	if (nrow(prevWethData) != 0) {
		#预测日最高气温
		h_temp <- prevWethData$MAX_TMP
		#预测日最低气温
		l_temp <- prevWethData$MIN_TMP	
	}else {
		#预测日最高气温
		h_temp <- NA
		#预测日最低气温
		l_temp <- NA
	}

	#预测日的星期类型	
	type_week <- weekdays(as.Date(preDate))
	#预测日所在月份	
	month <- months(as.Date(preDate))
	#预测日前一周最高气温


	return(c(type_week = type_week, month = month,h_temp = h_temp, l_temp = l_temp,  
					 h_temp_prev_week = h_temp_prev_week, l_temp_prev_week = l_temp_prev_week, 
					 h_temp_prev_week_sd = h_temp_prev_week_sd, l_temp_prev_week_sd = l_temp_prev_week_sd))
}

#基本负荷信息
logisticMethChoice_baseLoadInf <- function(preDate, loadData) {
	loadData$LOAD_DATE <- as.Date(loadData$LOAD_DATE)

	dates <- seq(as.Date(-7, origin=preDate),  as.Date(-1, origin=preDate),  by=1)
	tmpLoadData <- as.matrix.data.frame(loadData[which(loadData$LOAD_DATE%in%dates), ][-1])
	
	if (nrow(tmpLoadData) != 0) {
		#预测日前一周最高负荷
		h_load_prev_week <- max(tmpLoadData)
		#预测日前一周最低负荷
		l_load_prev_week <- min(tmpLoadData)
		#预测日前一周等单天最高负高荷的标准差
		h_load_prev_week_sd <- sd(apply(tmpLoadData, MARGIN = 1, FUN = max))
		#预测日前一周等单天最高低负荷的标准差
		l_load_prev_week_sd <- sd(apply(tmpLoadData, MARGIN = 1, FUN = min))
		#前一周平均负荷增长率
		aver_load <- apply(tmpLoadData, MARGIN = 1, FUN = mean)
		aver_grow_ratio_prev_week <- mean(diff(aver_load)/aver_load[seq(length(aver_load)-1)],
			na.rm = T)		
		}else {
			#预测日前一周最高负荷
			h_load_prev_week <- NA
			#预测日前一周最低负荷
			l_load_prev_week <- NA
			#预测日前一周等单天最高负高荷的标准差
			h_load_prev_week_sd <- NA
			#预测日前一周等单天最高低负荷的标准差
			l_load_prev_week_sd <- NA
			#前一周平均负荷增长率
			aver_grow_ratio_prev_week <- NA
		}


	return(c(h_load_prev_wee = h_load_prev_week, l_load_prev_week = l_load_prev_week, 
					 h_load_prev_week_sd = h_load_prev_week_sd, l_load_prev_week_sd = l_load_prev_week_sd,
					 aver_grow_ratio_prev_week = aver_grow_ratio_prev_week))
}

#前一周同一天温度信息
logisticMethChoice_prevSameDayLoadInf <- function(preDate, loadData) {
	tmpLoadData <- loadData[which(loadData$LOAD_DATE == (as.Date(preDate)-7)), ][-1]
	if (nrow(tmpLoadData) == 0) {
	  h_load_prev_sameday <- NA
	  l_load_prev_sameday <- NA
	}else {
	  h_load_prev_sameday <- max(tmpLoadData)
	  l_load_prev_sameday <- min(tmpLoadData) 
	}
	return(c(h_load_prev_sameday = h_load_prev_sameday, l_load_prev_sameday = l_load_prev_sameday))
}

#前一个月最相近日的气温信息
logisticMethChoice_similarDayLoadInf <- function(preDate, loadData,wethData, ahead) {
	similarDate <- logisticMethChoice_similarDay(preDate, wethData, ahead)
	tmpLoadData <- loadData[loadData$LOAD_DATE == similarDate, ][-1]

	if (nrow(tmpLoadData) != 0) {
		h_load_similarday <- max(tmpLoadData)
		l_load_similarday <- min(tmpLoadData)	
	}else {
		h_load_similarday <- NA
		l_load_similarday <- NA
	}

	return(c(h_load_similarday = h_load_similarday, l_load_similarday = l_load_similarday))
}

#预测日前5个同类型日气温信息整合
logisticMethChoice_sameDayLoadInf <- function(preDate, loadData) {
	#前五个同类型日日期
	sameDates <- logisticMethChoice_sameDay(preDate)
	#负荷数据日期预处理
	loadData$LOAD_DATE <- as.Date(loadData$LOAD_DATE)
	#前五个同类型日负荷数据
	sameDatesLoadData <- as.matrix.data.frame(loadData[which(loadData$LOAD_DATE%in%sameDates), ][-1])
	
	if (nrow(sameDatesLoadData) != 0) {
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
	}else {
		# 预测日前5个同类型日最高负荷	
		h_load_same_day <- NA
		# 预测日前5个同类型日最低负荷	
		l_load_same_day <- NA
		# 预测日前5个同类型日平均负荷	
		m_load_same_day <- NA
		# 预测日前5个同类型日最高负荷标准差	
		h_load_same_day_sd <- NA
		# 预测日前5个同类型日最低负荷标准差	
		l_load_same_day_sd <- NA
		# 预测日前5个同类型日平均负荷标准差	
		m_load_same_day_sd <- NA
	}


	loadInf <- c(h_load_same_day=h_load_same_day, l_load_same_day=l_load_same_day, 
		m_load_same_day=m_load_same_day, h_load_same_day_sd=h_load_same_day_sd, 
		l_load_same_day_sd=l_load_same_day_sd, m_load_same_day_sd=m_load_same_day_sd)
	return(loadInf)
}

#获取含有预测结果的表
logisticMethChoice_getPredTableName <- function(partName) {
  sql <- paste0("select table_name from information_schema.tables 
		where table_name like '%", partName, "%';")
  tableNames <- unlist(logisticMethChoice_exeDm2015(sql))
  
  return(unname(tableNames))
}

logisticMethChoice_sameTypeDate <- function(preDate, ahead) {
	preStartDate <- as.character(as.Date(preDate)-ahead+1)
	dates <- seq(as.Date(-31,origin = preDate), as.Date(-1,origin = preStartDate), 1)

	if (weekdays(as.Date(preDate))%in% c('星期六','星期日')) {
		sameTypeDays <- dates[(weekdays(as.Date(dates))%in%c('星期六','星期日'))]
	}else{
		sameTypeDays <- dates[!(weekdays(as.Date(dates))%in%c('星期六','星期日'))]
	}			 
	return(as.character(sameTypeDays))
}

logisticMethChoice_similarDay <- function(preDate, wethData, ahead) {
	#前一个月的同类型日（分周末和非周末）
	sameTypeDays <- logisticMethChoice_sameTypeDate(preDate, ahead)
	sTypeDayWethData <- wethData[which(wethData$WETH_DATE %in% sameTypeDays), ]
	preDateData <- unlist(wethData[wethData$WETH_DATE == preDate, ][c(2,3)])

	datecorr <- logisticMethChoice_dateCorr(sameTypeDays, preDate)
	tmpcorr <- logisticMethChoice_tempCorr(sTypeDayWethData, preDateData)
	return(sameTypeDays[which.max(datecorr*tmpcorr)])
}
#计算时间因子匹配系数
logisticMethChoice_dateCorr <- function(sameTypeDays,preDate,beta_1=.90) {
	n_day<-length(sameTypeDays)
	t<-as.numeric(as.Date(preDate)-as.Date(sameTypeDays))
	delta<-beta_1**(t%/%7)
	names(delta)<-sameTypeDays
	return(delta)
}

#计算历史日的温度数据与预测日的温度关联度
logisticMethChoice_tempCorr <- function(wethData,preDateData,alpha=0.5) {
	tmpData <- rbind(wethData$MIN_TMP, wethData$MAX_TMP)
	tmpData <- as.matrix(tmpData)
	tmp <- apply(tmpData, MARGIN = 2, FUN = function(x) {return(abs(x-preDateData))})
	tmp_scale <- apply(tmp,1,FUN=function(x) {return((x-min(x))/(max(x)-min(x)))})
	res <- (min(tmp_scale)+alpha*max(tmp_scale))/(tmp_scale+alpha*max(tmp_scale))
 	res <- apply(res,MARGIN = 1,sum)/2
 	names(res) <- wethData$WETH_DATE
 	return(res)
}

logisticMethChoice_bestModel <- function(preDate, wethData, loadData, ahead, modelPath) {
	
	wethData$WETH_DATE <- as.character(wethData$WETH_DATE)
	loadData$LOAD_DATE <- as.character(loadData$LOAD_DATE)
	#提取7个预测日的特征
	featureSet <- NULL
	for (j in 1:7) {
		tmpDate <- as.character(as.Date(preDate)+j-1)
		#气温参数
		wethInf <- logisticMethChoice_wethInf(tmpDate, wethData, j)
		loadInf <- logisticMethChoice_loadInf(tmpDate, loadData, wethData, j)

		tmp <- NULL
		tmp <- append(wethInf, loadInf)
		if (is.null(featureSet)) {
			featureSet <-array(tmp)
		}else{
			featureSet <- rbind(featureSet, tmp)	
		}	
	}
	featureSet <- as.data.frame.array(featureSet)
	row.names(featureSet) <- NULL
	for (i in 3:ncol(featureSet)) {
		featureSet[[i]] <- as.numeric(as.character(featureSet[[i]]))
	}
	
	# featureSet <- na.omit(featureSet)

	#载入模型
	load(modelPath)
	#计算对于每天各模型成功概率
	probMat <- sapply(models, FUN = function(model) {
		predict(object = model, newdata = featureSet, type = 'response')
		})
	#计算概率最大的模型
	modelNames <- colnames(probMat)
	indexOfMaxProb <- apply(probMat, 1, which.max)
	return(modelNames[indexOfMaxProb])
}