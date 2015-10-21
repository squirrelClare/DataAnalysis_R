mydata<-read.csv('D:/method_choice_4.csv')
mydata$X<-NULL

library(rpart)
library(rpart.plot)
library(rattle)
tableNames<-as.character(methChoice_getPredTableName())
mydata$method.rates_s_1<-tableNames[mydata$method.rates_s_1]
mydata$method.rates_s_1<-factor(mydata$method.rates_s_1)
mydata$method.rates_s_1<-as.factor(mydata$method.rates_s_1)

ct <- rpart.control(xval=10, minsplit=2, cp=0.1,maxdepth = 10)

model<-rpart(formula = method.rates_s_1~h_temp+l_temp+type_week+month+h_temp_prev_week+
	l_temp_prev_week+h_temp_prev_week_sd+l_temp_prev_week_sd+
	h_load_prev_week+l_load_prev_week+h_load_prev_week_sd+l_load_prev_week_sd,
	data=mydata,method='class',control = ct,minsplit=200)

rpart.plot(model, branch=1, branch.type=2, type=1, extra=102,
           shadow.col="gray", box.col="green",
           border.col="blue", split.col="red",
           split.cex=1.2, main="Kyphosis决策树");
fancyRpartPlot(model)
