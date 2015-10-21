# loadData=read.csv("C:\\Users\\dell\\Desktop\\data_outpu.csv",header=T)
# loadData<-loadData[4:100]
# loadData$LOAD_DATE<-strftime(strptime(loadData$LOAD_DATE,format = '%Y/%m/%d'),format = '%Y-%m-%d')
# 
# wethData=read.csv("C:\\Users\\dell\\Desktop\\福田区_竹子林基地.csv",header=T)
# wethData$X=NULL
# wethData$WETH_DATE<-strftime(strptime(wethData$WETH_DATE,format = '%Y/%m/%d'),format = '%Y-%m-%d')
# 
# holiday_init()
# models=holiday_buildModel(loadData,wethData,startdate="2012-09-01",enddate="2014-08-20")
# preds=holiday_prediction(models,loadData,wethData,predate="2014-05-01",ahead=3)

loadData=read.csv("C:\\Users\\dell\\Desktop\\data_outpu.csv",header=T)
loadData$X=NULL
loadData<-loadData[4:100]
loadData$LOAD_DATE=as.Date(loadData$LOAD_DATE)
loadData=loadData[order(loadData$LOAD_DATE),]
wethData=read.csv("C:\\Users\\dell\\Desktop\\福田区_竹子林基地.csv",header=T)
wethData$X=NULL
wethData$WETH_DATE=as.Date(wethData$WETH_DATE)
holiday_init()
models=holiday_buildModel(loadData,wethData,startdate="2012-09-01",enddate="2014-08-20")
preds=holiday_prediction(models,loadData,wethData,predate="2014-05-01",ahead=3)
