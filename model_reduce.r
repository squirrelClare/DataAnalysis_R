x <- bestFit
x$residuals <- NULL 
x$fitted.values <- NULL 
x$effects <- NULL 
x$R <- NULL 
x$anova <- NULL
x$data <- NULL
x$model <- NULL
x$linear.predictors <- NULL
x$y <- NULL
x$weights <- NULL
x$prior.weights <- NULL
x$

y <- NULL
y <- predict(object = x, newdata = testData, type = 'response')
sum(y == pred)


fileName <- names(bestFit)
for ( i in 1:length(bestFit)) {
	tmp <- bestFit[i]
	save(tmp,file = paste0('E:/新特征集/',fileName[i],'.RData'))
}


sTime <- Sys.time()
load('D:/test1.RData')
print(Sys.time()-sTime)



res <- sapply(models, FUN = function(model){
	predict(object = model, newdata = srcData, type = 'response')
})

modelNames <- colnames(res)
indexOfMaxProb <- apply(res, 1, which.max)



load('D:/bdp2.rd')
preDate="2014-09-01"
wethData=bdp2$wethData
loadData=bdp2$loadData
ahead=7
modelPath="D:/预测结果/models.Rdata"


a=logisticMethChoice_bestModel(preDate="2014-09-01",
	wethData=bdp2$wethData, loadData=bdp2$loadData, ahead=7, modelPath="D:/预测结果/models.Rdata")


