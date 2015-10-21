##
#创建时间：2015-7-8
#功能：计算已有8中模型对单个台区单天预测成功概率，成功定义为平均误差率低于10%
##

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

#获取指定表中的天气数据
logisticMethChoice_getWethData <- function(wethDataTableName) {
	sql <- paste0("select * from ",wethDataTableName,";")
	return(logisticMethChoice_exeLoadmonitoring(sql))
}

#生成单个模型的特征表，存放于当前目录下的./ratio文件夹中
logisticMethChoice_generatTable <- function(modelName, realLoadTableName, wethDataTableName) {
	ratioData <- read.csv(file = paste0('./ratio/', modelName, '_errorRatio.csv'))
	ratioData$row_names <- NULL
	ratioData <- na.omit(ratioData)
	#预测起始，结束日期
	fDate <- as.character(min(as.Date(ratioData$LOAD_DATE)))
	tDate <- as.character(max(as.Date(ratioData$LOAD_DATE)))
	#预测负荷重的变压器编号
	ids <- unique(ratioData$location_id)
	#预测日前60天真实负荷数据
	
	fDateOfReal <- as.character(as.Date(-60, origin = fDate)) 	#选取真实负荷的起始日
	tDateOfReal <- as.character(as.Date(-1, origin = fDate))		##选取真实负荷的终止日
	realLoadData <- logisticMethChoice_getRealLoadData(fDateOfReal, tDateOfReal, ids, realLoadTableName)
	#重新选择预测负荷重个台区百分误差率
	ratioData <- ratioData[which(ratioData$location_id %in% realLoadData$location_id), ]
	#预测天数
	ahead <- as.numeric(as.Date(tDate) - as.Date(fDate)) + 1
	#天气数据
	wethData <- logisticMethChoice_getWethData(wethDataTableName)
	wethData <- wethData[order(as.Date(wethData$weather_date)), ]

	#初始化最终结果存储数据框
	result <-NULL
	for (id in unique(ratioData$location_id)) {		
		#获取台区负荷数据
		loadData <- realLoadData[which(realLoadData$location_id == id), ]
		for (j in 1:ahead) {
			tmpDate <- as.character(as.Date(fDate)+j-1)
			#气温参数
			wethInf <- logisticMethChoice_wethInf(tmpDate, wethData, j)
			loadInf <- logisticMethChoice_loadInf(tmpDate, loadData, wethData, j)
			
			#查询当前台区在当前日期的平均误差率
			indexInRatioData <- which(ratioData$location_id == id & ratioData$LOAD_DATE == tmpDate)
			meanRatio <- ifelse(length(indexInRatioData) == 0, NA, ratioData[indexInRatioData, ]$meanRatio)

			tmp <- NULL
			tmp <- c(id = id,LOAD_DATE = tmpDate)
			tmp <- append(tmp, wethInf)
			tmp <- append(tmp, loadInf)
			#设置预测成功定义
			tmp <- append(tmp, c(success = ifelse(meanRatio <= 0.10, 'yes', 'no')))
			if (is.null(result)) {
				result <-array(tmp)
				}else{
					result <- rbind(result, tmp)	
				}	
			}
		}
		result <- as.data.frame.array(result)

	#将数据框中本应为数值的特征从因子转化为数值
	for (i in 5:(ncol(result)-1)) {
		result[[i]] <- as.numeric(as.character(result[[i]]))
	}
	row.names(result) <- NULL
	write.csv(x = result,file = paste0('./features/',modelName,'_features.csv'))		#写入本地
}
#获取指定日期前五个同类型日
logisticMethChoice_sameDay <- function(preDate) {
	dates <- seq(as.Date(-38, origin=preDate),  as.Date(-1, origin = preDate),  by=1)
	return(dates[which(weekdays(dates)==weekdays(as.Date(preDate)))])
}

#预测日前5个同类型日气温信息整合
logisticMethChoice_sameDayWethInf <- function(preDate, wethData) {
	#天气数据预处理
	wethData$weather_date <- as.Date(wethData$weather_date)
	#前五个同类型日日期
	sameDates <- logisticMethChoice_sameDay(preDate)
	#前五个同类型日天气数据
	sameDatesWethData <- wethData[which(wethData$weather_date%in%sameDates), ]

	if (nrow(sameDatesWethData) != 0) {
		# 预测日前5个同类型日最高气温	
		h_temp_same_day <- max(sameDatesWethData$max_temperature)
		# 预测日前5个同类型日最低气温	
		l_temp_same_day <- max(sameDatesWethData$min_temperature)
		# 预测日前5个同类型日平均气温	
		m_temp_same_day <- mean(append(wethData$max_temperature, wethData$min_temperature))
		# 预测日前5个同类型日最高气温标准差	
		h_temp_same_day_sd <- sd(sameDatesWethData$max_temperature)
		# 预测日前5个同类型日最低气温标准差	
		l_temp_same_day_sd <- sd(sameDatesWethData$min_temperature)
		# 预测日前5个同类型日平均气温标准差	
		m_temp_same_day_sd <- sd(apply(X=cbind(sameDatesWethData$max_temperature, 
			sameDatesWethData$min_temperature), MARGIN=1, FUN=mean))
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
	wethData$weather_date <- as.Date(wethData$weather_date)
	#前一周同一天气温数据
	tmpWethData <- wethData[which(wethData$weather_date == as.character(as.Date(-7,origin = preDate))),]
	if (nrow(tmpWethData) != 0) {
		#前一周同一天最高气温
		h_temp_prev_sameday <- tmpWethData$max_temperature
		#前一周同一天最低气温
		l_temp_prev_sameday <- tmpWethData$min_temperature
		}else {
			h_temp_prev_sameday <- NA
			l_temp_prev_sameday <- NA
		}
		return(c(h_temp_prev_sameday = h_temp_prev_sameday, l_temp_prev_sameday = l_temp_prev_sameday))
	}
#前一个月最相近日的气温信息
logisticMethChoice_similarDayWethInf <- function(preDate, wethData, ahead) {
	similarDate <- logisticMethChoice_similarDay(preDate, wethData, ahead)
	tmpWethData <- wethData[wethData$weather_date == similarDate, ]
	if (nrow(tmpWethData) != 0) {
		#最高气温
		h_temp_similarday <- tmpWethData$max_temperature
		#最低气温
		l_tem_similarday <- tmpWethData$min_temperature
		}else {
			h_temp_similarday <- NA
			l_tem_similarday <- NA
		}

		return(c(h_temp_similarday = h_temp_similarday, l_tem_similarday = l_tem_similarday))
	}
#基本气温数据信息
logisticMethChoice_baseWethInf <- function(preDate, wethData) {
	#天气数据预处理
	wethData$weather_date <- as.Date(wethData$weather_date)
	
	#预测日前一周日期
	dates <- seq(as.Date(-7, origin=preDate),  as.Date(-1, origin=preDate),  by=1)
	#预测日前一周气温数据
	tmpWethData <- wethData[which(wethData$weather_date %in% dates), ]
	#预测日当天气温数据
	prevWethData <- wethData[which(wethData$weather_date == as.Date(preDate)), ]
	
	if (nrow(tmpWethData) != 0) {
		h_temp_prev_week <- max(tmpWethData$max_temperature)
		#预测日前一周最低气温	
		l_temp_prev_week <- min(tmpWethData$min_temperature)
		#预测日前一周最高气温方差（每天最高气温）	
		h_temp_prev_week_sd <- sd(tmpWethData$max_temperature)
		#预测日前一周最低气温方差（每天最低气温）	
		l_temp_prev_week_sd <- sd(tmpWethData$min_temperature)
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
		h_temp <- prevWethData$max_temperature
		#预测日最低气温
		l_temp <- prevWethData$min_temperature	
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
	loadData$real_time <- as.Date(loadData$real_time)

	dates <- seq(as.Date(-7, origin=preDate),  as.Date(-1, origin=preDate),  by=1)
	tmpLoadData <- na.omit(as.matrix.data.frame(loadData[which(loadData$real_time%in%dates), ][2:97]))
	
	#空值处理
	tmpLoadData <- na.omit(tmpLoadData)

	if (nrow(tmpLoadData) != 0) {

		meanValue <- mean(tmpLoadData, na.rm = T)
		#预测日前一周最高负荷
		h_load_prev_week_cv <- max(tmpLoadData)/meanValue
		#预测日前一周最低负荷
		l_load_prev_week_cv <- min(tmpLoadData)/meanValue
		#预测日前一周等单天最高负高荷的标准差
		maxLoads <- na.omit(apply(tmpLoadData, MARGIN = 1, FUN = max))
		# h_load_prev_week_sd_cv <- sd(maxLoads)/mean(maxLoads)
		h_load_prev_week_sd_cv <- sd(maxLoads)/meanValue
		#预测日前一周等单天最高低负荷的标准差
		minLoads <- na.omit(apply(tmpLoadData, MARGIN = 1, FUN = min))
		# l_load_prev_week_sd_cv <- sd(minLoads)/mean(minLoads)
		l_load_prev_week_sd_cv <- sd(minLoads)/meanValue
		#前一周平均负荷增长率
		aver_load <- apply(tmpLoadData, MARGIN = 1, FUN = mean)
		aver_grow_ratio_prev_week <- mean(diff(aver_load)/aver_load[seq(length(aver_load)-1)],
			na.rm = T)		
		}else {
			#预测日前一周最高负荷
			h_load_prev_week_cv <- NA
			#预测日前一周最低负荷
			l_load_prev_week_cv <- NA
			#预测日前一周等单天最高负高荷的标准差
			h_load_prev_week_sd_cv <- NA
			#预测日前一周等单天最高低负荷的标准差
			l_load_prev_week_sd_cv <- NA
			#前一周平均负荷增长率
			aver_grow_ratio_prev_week <- NA
		}


		return(c(h_load_prev_week_cv = h_load_prev_week_cv, l_load_prev_week_cv = l_load_prev_week_cv, 
			h_load_prev_week_sd_cv = h_load_prev_week_sd_cv, l_load_prev_week_sd_cv = l_load_prev_week_sd_cv,
			aver_grow_ratio_prev_week = aver_grow_ratio_prev_week))
	}

#前一周同一天温度信息
logisticMethChoice_prevSameDayLoadInf <- function(preDate, loadData) {
	tmpLoadData <- loadData[which(loadData$real_time == (as.Date(preDate)-7)), ][2:97]
	if (nrow(tmpLoadData) == 0) {
		h_load_prev_sameday_cv <- NA
		l_load_prev_sameday_cv <- NA
		}else {
			tmpLoadData <- unlist(tmpLoadData)
			meanValue <- mean(tmpLoadData)

			h_load_prev_sameday_cv <- max(tmpLoadData)/meanValue
			l_load_prev_sameday_cv <- min(tmpLoadData)/meanValue
		}

		return(c(h_load_prev_sameday_cv = h_load_prev_sameday_cv, l_load_prev_sameday_cv = l_load_prev_sameday_cv))
	}

#前一个月最相近日的气温信息
logisticMethChoice_similarDayLoadInf <- function(preDate, loadData,wethData, ahead) {
	similarDate <- logisticMethChoice_similarDay(preDate, wethData, ahead)
	tmpLoadData <- loadData[loadData$real_time == similarDate, ][2:97]

	if (nrow(tmpLoadData) != 0) {
		tmpLoadData <- unlist(tmpLoadData)
		meanValue <- mean(tmpLoadData, na.rm = T)
		h_load_similarday_cv <- max(tmpLoadData, na.rm = T)/meanValue
		l_load_similarday_cv <- min(tmpLoadData, na.rm = T)/meanValue	
		}else {
			h_load_similarday_cv <- NA
			l_load_similarday_cv <- NA
		}

		return(c(h_load_similarday_cv = h_load_similarday_cv, l_load_similarday_cv = l_load_similarday_cv))
	}

#预测日前5个同类型日气温信息整合
logisticMethChoice_sameDayLoadInf <- function(preDate, loadData) {
	#前五个同类型日日期
	sameDates <- logisticMethChoice_sameDay(preDate)
	#负荷数据日期预处理
	loadData$real_time <- as.Date(loadData$real_time)
	#前五个同类型日负荷数据
	sameDatesLoadData <- as.matrix.data.frame(loadData[which(loadData$real_time%in%sameDates), ][2:97])
	
	#空值处理
	sameDatesLoadData <- na.omit(sameDatesLoadData)

	if (nrow(sameDatesLoadData) != 0) {

		meanValue <- mean(sameDatesLoadData)
		# 预测日前5个同类型日最高负荷	
		h_load_same_day_cv <- max(sameDatesLoadData)/meanValue
		# 预测日前5个同类型日最低负荷	
		l_load_same_day_cv <- min(sameDatesLoadData)/meanValue
		# 预测日前5个同类型日最高负荷标准差	
		maxLoads <- apply(sameDatesLoadData, MARGIN = 1, FUN = max)
		# h_load_same_day_sd_cv <- sd(maxLoads)/mean(maxLoads)
		h_load_same_day_sd_cv <- sd(maxLoads)/meanValue
		# 预测日前5个同类型日最低负荷标准差	
		minLoads <- apply(sameDatesLoadData, MARGIN = 1, FUN = min)
		# l_load_same_day_sd_cv <- sd(minLoads)/mean(minLoads)
		l_load_same_day_sd_cv <- sd(minLoads)/meanValue
		# 预测日前5个同类型日平均负荷标准差	
		meanLoads <- apply(sameDatesLoadData, MARGIN = 1, FUN = mean)
		# m_load_same_day_sd_cv <- sd(meanLoads)/mean(meanLoads)
		m_load_same_day_sd_cv <- sd(meanLoads)/meanValue
		}else {
		# 预测日前5个同类型日最高负荷	
		h_load_same_day_cv <- NA
		# 预测日前5个同类型日最低负荷	
		l_load_same_day_cv <- NA
		# 预测日前5个同类型日平均负荷	
		m_load_same_day <- NA
		# 预测日前5个同类型日最高负荷标准差	
		h_load_same_day_sd_cv <- NA
		# 预测日前5个同类型日最低负荷标准差	
		l_load_same_day_sd_cv <- NA
		# 预测日前5个同类型日平均负荷标准差	
		m_load_same_day_sd_cv <- NA
	}


	loadInf <- c(h_load_same_day_cv=h_load_same_day_cv, l_load_same_day_cv=l_load_same_day_cv, 
		h_load_same_day_sd_cv=h_load_same_day_sd_cv, l_load_same_day_sd_cv=l_load_same_day_sd_cv, 
		m_load_same_day_sd_cv=m_load_same_day_sd_cv)
	return(loadInf)
}

#训练逻辑回归模型
logisticMethChoice_perModelTrain <- function(modelName) {
	srcData <- read.csv(paste0('./features/',modelName,'_features.csv'))
	
	indexOfYes <- which(srcData$success == 'yes')
	indexOfNo <- which(srcData$success == 'no')
	numOfTrainYes <- floor(length(indexOfYes)*3/4)
	
	
	indexOfSampleYes <- sort(sample(indexOfYes, size = numOfTrainYes))
	indexOfSampleNo <- sort(sample(indexOfNo, size = numOfTrainYes))
	trainData <- srcData[c(indexOfSampleYes,indexOfSampleNo), ]
	testData <- srcData[-c(indexOfSampleYes,indexOfSampleNo),]

	trainData$X <- NULL
	trainData$vkont <- NULL
	trainData$LOAD_DATE <- NULL

	trainData$month <- NULL
	testData$month <- NULL
	testData$X <- NULL
	testData$vkont <- NULL
	testData$LOAD_DATE <- NULL

	trainData <- na.omit(trainData)
	testData <- na.omit(testData)
	fit <- glm(success~., data = trainData, family = binomial(link = 'logit'))

	library(MASS)
	bestFit <- stepAIC(object = fit, direction = 'both')
	pred <- predict(object = bestFit, newdata = testData, type = 'response')
	pred <- ifelse(pred > .5, 'yes', 'no')
	tableMat <- table(src = testData$success,dst = pred)
	dstTable <- prop.table(tableMat,margin = 1)

	bestFit$residuals <- NULL 
	bestFit$fitted.values <- NULL 
	bestFit$effects <- NULL 
	bestFit$R <- NULL 
	bestFit$anova <- NULL
	bestFit$data <- NULL
	bestFit$model <- NULL
	bestFit$linear.predictors <- NULL
	bestFit$y <- NULL
	bestFit$weights <- NULL
	bestFit$prior.weights <- NULL


	write.csv(x = dstTable,file = paste0('./modelandresult/',modelName,'_table.csv'))
	write.csv(x = bestFit$coefficients,file = paste0('./modelandresult/',modelName,'_ceoff.csv'))
	return(bestFit)
}

#获取与预测日相同类型的日期
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
	sTypeDayWethData <- wethData[which(wethData$weather_date %in% sameTypeDays), ]
	preDateData <- unlist(wethData[wethData$weather_date == preDate, ][c(2,3)])

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
	ratio <- rbind(wethData$min_temperature, wethData$max_temperature)
	ratio <- as.matrix(ratio)
	tmp <- apply(ratio, MARGIN = 2, FUN = function(x) {return(abs(x-preDateData))})
	tmp_scale <- apply(tmp,1,FUN=function(x) {return((x-min(x))/(max(x)-min(x)))})
	res <- (min(tmp_scale)+alpha*max(tmp_scale))/(tmp_scale+alpha*max(tmp_scale))
	res <- apply(res,MARGIN = 1,sum)/2
	names(res) <- wethData$weather_date
	return(res)
}
#按模型名称将预测结果分组并写入8个csv文件
logisticMethChoice_mergePredictData <- function(dirName) {
	fullFileNames <- dir(path = dirName, full.names = T)
	fileNames <- dir(path = dirName)
	
	#八种模型的名称
	modelNames <- c("cycleBase","cycleMulti","extendOut","loadModule1","loadModule2",
		"loadMulti","simiExtend","wthCycle")
	
	cycleBase <- NULL
	cycleMulti <- NULL
	extendOut <- NULL
	loadModule1 <- NULL
	loadModule2 <- NULL
	loadMulti <- NULL
	simiExtend <- NULL
	wthCycle <- NULL

	for (i in seq(length(fileNames))) {
		fileName <- fileNames[i]
		fullFileName <- fullFileNames[i]

		tmp <- unlist(strsplit(x = fileName, split = '~'))
		
		id <- tmp[1]
		fDate <- tmp[2]
		nDay <- as.numeric(tmp[3])
		modelName <- unlist(strsplit(tmp[4], split = "\\."))[1]
		
		indexOfMOdel <- which(modelNames == modelName)
		
		ratio <- NULL
		ratio <- read.csv(fullFileName)
		ratio$X <- NULL
		if (indexOfMOdel == 1) {
			cycleBase <- rbind(cycleBase, cbind(location_id = rep(id, nDay),ratio))
		}
		if (indexOfMOdel == 2) {
			cycleMulti <- rbind(cycleMulti, cbind(location_id = rep(id, nDay),ratio))
		}
		if (indexOfMOdel == 3) {
			extendOut <- rbind(extendOut, cbind(location_id = rep(id, nDay),ratio))
		}
		if (indexOfMOdel == 4) {
			loadModule1 <- rbind(loadModule1, cbind(location_id = rep(id, nDay),ratio))
		}	
		if (indexOfMOdel == 5) {
			loadModule2 <- rbind(loadModule2, cbind(location_id = rep(id, nDay),ratio))
		}	
		if (indexOfMOdel == 6) {
			loadMulti <- rbind(loadMulti, cbind(location_id = rep(id, nDay),ratio))
		}	
		if (indexOfMOdel == 7) {
			simiExtend <- rbind(simiExtend, cbind(location_id = rep(id, nDay),ratio))
		}
		if (indexOfMOdel == 8) {
			wthCycle <- rbind(wthCycle, cbind(location_id = rep(id, nDay),ratio))
		}		
	}
	write.csv(x = cycleBase, paste0('./loadData/cycleBase.csv'))
	write.csv(x = cycleMulti, paste0('./loadData/cycleMulti.csv'))
	write.csv(x = extendOut, paste0('./loadData/extendOut.csv'))
	write.csv(x = loadModule1, paste0('./loadData/loadModule1.csv'))
	write.csv(x = loadModule2, paste0('./loadData/loadModule2.csv'))
	write.csv(x = loadMulti, paste0('./loadData/loadMulti.csv'))
	write.csv(x = simiExtend, paste0('./loadData/simiExtend.csv'))
	write.csv(x = wthCycle, paste0('./loadData/wthCycle.csv'))
}

# 在数据库DM2015中执行sql，并返回查询的结果
logisticMethChoice_exeLoadmonitoring <- function(sql) {
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
logisticMethChoice_getPredictLoadData <- function(modelName) {
	predictLoadData <- read.csv(paste0('./ratio/',modelName,'.csv'))
	predictLoadData$X <- NULL
	return(predictLoadData)
}

#获取真实负荷数据
logisticMethChoice_getRealLoadData <- function(fDate, tDate, ids, realLoadTableName) {
	sql <- paste0("select * from ", realLoadTableName, " where real_time >= '", fDate, "' and real_time <= '",
		tDate, "' and location_id in ('", paste0(ids, collapse = "','"), "') order by location_id, real_time;")
	realLoadData <- logisticMethChoice_exeLoadmonitoring(sql)
	return(realLoadData)
}
#

#计算并保存误差率
logisticMethChoice_errorRatioPerModel <- function(preLoad, realLoadTableName) {
	forecastDate <- as.Date(preLoad$LOAD_DATE)
	fDate <-as.character(min(forecastDate))	#预测起始日
	tDate <-as.character(max(forecastDate))	#预测终止日

	idsOfPreLoad <- unique(preLoad$location_id)		#预测表中的id
	sql <- paste0("select distinct(location_id) from ",realLoadTableName,";")
	idsOfRealLoad <-unique(unlist(logisticMethChoice_exeLoadmonitoring(sql)))	#获取真实表中的id
	idsOfIntersect <- intersect(idsOfRealLoad, idsOfPreLoad)   #获取真实值和预测值表中的公共id

	realLoad <- logisticMethChoice_getRealLoadData(fDate, tDate, idsOfIntersect, realLoadTableName)
	realLoadData <- realLoadData[order(realLoadData$real_time), ]
	preLoad <- preLoad[which(preLoad$location_id %in% idsOfIntersect), ]

	ratioDataFrame <- NULL

	#逐个id计算误差率
	for (id in idsOfIntersect) {
		tmp <- logisticMethChoice_getMeanErroRatioOfPerId(realLoad, preLoad, fDate, tDate, id)
		if (! is.null(tmp)) {
			ratioDataFrame <- rbind(ratioDataFrame, tmp)
		}	
	}
	return(ratioDataFrame)
}
#计算单个专变或者台区的平均误差率
logisticMethChoice_getMeanErroRatioOfPerId <- function(reaLoadData, preLoadData, fDate, tDate, id) {
	nDay <- as.numeric(as.Date(tDate) - as.Date(fDate)) + 1

	real <- reaLoadData[which(reaLoadData$location_id == id), ]	#读取真实负荷表中的数据
	real <- real[order(real$real_time), ]
	pre <- preLoadData[which(preLoadData$location_id == id), ]	#读取预测负荷表中的数据
	pre <- pre[order(pre$LOAD_DATE), ]
	if (nrow(real) != nDay | nrow(pre) != nDay) {
		return(NULL);
	} 
 	real <- real[2:97]	#提取原始负荷数据
	pre <- pre[3:98]	#提取预测负荷数据 	

  	#误差率计算
  	ratioData <- sapply(X = seq(ncol(pre)), FUN=function(x) {
  		errorRatio <- abs(pre[[x]]-real[[x]])/real[[x]]	
  		errorRatio[which(errorRatio == Inf)] <- NA
  		return(errorRatio)
  		})
	meanRatioData <- round(apply(X = ratioData, MARGIN = 1, FUN = mean, na.rm=T),digits = 4)		#平均误差率计算
	#结果整合为一个数据框
	ratioIdDate <- data.frame(location_id = rep(id, nDay), LOAD_DATE = seq(as.Date(fDate), as.Date(tDate), 1), 
		meanRatio = meanRatioData)
	return(ratioIdDate)
}

#统计每一种模型的预测误差率并保存到当前工作目录下的/ratio文件夹下
logisticMethChoice_staticSavePerModel <- function(realLoadTableName) {
	print('START!!!')
	#八种模型的名称
	modelNames <- c("cycleBase","cycleMulti","extendOut","loadModule1","loadModule2",
		"loadMulti","simiExtend","wthCycle")
	
	for (modelName in modelNames) {
		preLoad <- read.csv(file = paste0('./loadData/', modelName, '.csv'))
		
		preLoad$X <- NULL
		errorRatioData <- logisticMethChoice_errorRatioPerModel(preLoad, realLoadTableName)
		write.csv(errorRatioData, paste0('./ratio/', modelName, '_errorRatio.csv'))
	}
	print('SUCCESS!!!')
}

#生成所有模型的特征表
logisticMethChoice_generatAllTable<- function(modelNames, realLoadTableName, wethDataTableName) {
	for (modelName in modelNames) {
		logisticMethChoice_generatTable(modelName, realLoadTableName, wethDataTableName)
	}
}
logisticMethChoice_modelsTrain<- function() {
	#八种模型的名称
	modelNames <- c("cycleBase","cycleMulti","extendOut","loadModule1","loadModule2",
		"loadMulti","simiExtend","wthCycle")
	
	models <- list()
	
	for (i in 1:length(modelNames)) {
		modelName <- modelNames[i]
		models[[modelName]] <- logisticMethChoice_perModelTrain(modelName)
	}
	
	save(models,file = paste0('./modelandresult/','models.Rdata'))
}
logisticMethChoice_main <- function(dirName) {
	if (!file.exists('./loadData')) {
		dir.create('./loadData')
	}
	if (!file.exists('./ratio')) {
		dir.create('./ratio')
	}
	if (!file.exists('./features')) {
		dir.create('./features')
	}
	if (!file.exists('./modelandresult')) {
		dir.create('./modelandresult')
	}

	#八种模型的名称
	modelNames <- c("cycleBase","cycleMulti","extendOut","loadModule1","loadModule2",
		"loadMulti","simiExtend","wthCycle")

	#按预测模型的不同合并预测负荷数据
	logisticMethChoice_mergePredictData(dirNameOfPredicLoad)
	#结合真实负荷数据计算预测负荷的百分误差率
	logisticMethChoice_staticSavePerModel(realLoadTableNameOfPredicTime)
	#针对8个模型建立各自的特征表
	logisticMethChoice_generatAllTable(modelNames, realLoadTableNameOfHistoryTime, wethDataTableName)
	#对八个模型训练选择模型
	logisticMethChoice_modelsTrain()
}  