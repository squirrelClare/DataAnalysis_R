##
#功能：
#		合并指定目录下的数据框
#参数：
#		dirName：目标目录
#返回：
#		resultData：整合后的数据框
##
predictionStatic_mergeDataFrame <- function(dirName) {
  setwd(dirName)
  fileNames <- dir(path = dirName, full.name =T)
  for (i in seq(length(fileNames))) {
    if (i == 1) {
      resultData <- read.csv(fileNames[i])
    } else {
      resultData <- rbind(resultData, read.csv(fileNames[i]))
    }
  }
  resultData <- na.omit(resultData)
  return(resultData)
}

# 从源数据框获取指定id的记录
predictionStatic_getPartData <- function(srcDataFrame, idChoice) {
  resultData <- srcDataFrame[which(srcDataFrame$location_id %in% idChoice), ]
  return(resultData)
}

# 从源数据框获取专变的记录
predictionStatic_getErrorDataOfZhuanbian <- function(srcDataFrame, idOfZhuanbian) {
  resultData <- predictionStatic_getPartData(srcDataFrame, idOfZhuanbian)
  return(resultData)
}

# 从源数据框获取台区的记录
predictionStatic_getErrorDataOfTaiqu <- function(srcDataFrame, idOfTaiqu) {
  resultData <- predictionStatic_getPartData(srcDataFrame, idOfTaiqu)
  return(resultData)
}

# 从源数据框获取福田区的台区记录
predictionStatic_getErrorDataOfTaiquInFutian <- function(srcDataFrame, idOfTaiquInFutian) {
  resultData <- predictionStatic_getPartData(srcDataFrame, idOfTaiquInFutian)
  return(resultData)
}

predictionStatic_splitData <- function() {
  srcDataFrame <- predictionStatic_mergeDataFrame('D:/f20001to30000')		#读取误差率源数据
  srcDataFrame$X <- NULL
  
  idTable <- read.csv('D:/device_section_map2.csv')	#读取编号、地区、变压器类型对照表
  idTableOfZhuanbianAndTaiqu <- split(idTable, idTable$VTYPE)		#按变压器类型分割编号
  
  idOfTaiqu <- idTableOfZhuanbianAndTaiqu[['taiqu']]		#获取台区编号
  idOfZhuanbian <- idTableOfZhuanbianAndTaiqu[['zhuanbian']]		#获取专变编号
  idOfTaiquInFutian <- idOfTaiqu[which(idOfTaiqu$VSECTION == '福田'), ]		#获取福田区台区编号
  
  #获取三组数据
  dataOfTaiqu <- predictionStatic_getErrorDataOfTaiqu(srcDataFrame, idOfTaiqu$DEVICE_ID)
  dataOfZhuanbian <- predictionStatic_getErrorDataOfZhuanbian(srcDataFrame, idOfZhuanbian$DEVICE_ID)
  dataOfTaiquInFutian <- predictionStatic_getErrorDataOfTaiquInFutian(srcDataFrame, idOfTaiquInFutian$DEVICE_ID)
  
  write.csv(x = dataOfTaiqu, file = 'D:/dataOfTaiqu.csv')
  write.csv(x = dataOfZhuanbian, file = 'D:/dataOfZhuanbian.csv')
  write.csv(x = dataOfTaiquInFutian, file = 'D:/dataOfTaiquInFutian.csv')
  
}