logisticMethChoice_sameTypeDate <- function(preDate) {
	dates <- seq(as.Date(-31,origin = preDate), as.Date(-1,origin = preDate), 1)

	if (weekdays(as.Date(preDate))%in%c('星期六','星期日')) {
		sameTypeDays <- dates[(weekdays(as.Date(dates))%in%c('星期六','星期日'))]
	}else{
		sameTypeDays <- dates[!(weekdays(as.Date(dates))%in%c('星期六','星期日'))]
	}			 
	return(as.character(sameTypeDays))
}

logisticMethChoice_globalCoef <- function(preDate, wethData) {
	#前一个月的同类型日（分周末和非周末）
	sameTypeDays <- logisticMethChoice_sameTypeDate(preDate)
	sTypeDayWethData <- wethData[which(wethData$WETH_DATE %in% sameTypeDays), ]
	preDateData <- unlist(wethData[wethData$WETH_DATE == preDateData, ])

	datecorr <- logisticMethChoice_dateCorr(sameTypeDays, preDate)
	tmpcorr <- logisticMethChoice_tempCorr(sTypeDayWethData, preDateData)
	return(names(max(datecorr*tmpcorr)))

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