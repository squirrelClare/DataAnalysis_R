mydata<-read.csv('D:/method_choice_5.csv')
mydata$X<-NULL
mydata$flag_id1s<-NULL
for (i in 2:ncol(mydata)) {
	mydata[[i]][which(mydata[[i]]==0)]<-NA
}
mydata <- na.omit(mydata)

library(RWeka)
mydata$method.rates_s_1<-as.factor(mydata$method.rates_s_1)

# fit<-J48(formula = method.rates_s_1~h_temp+l_temp+type_week+month+h_temp_prev_week+
# 						 	l_temp_prev_week+h_temp_prev_week_sd+l_temp_prev_week_sd+
# 						 	h_load_prev_week+l_load_prev_week+h_load_prev_week_sd+l_load_prev_week_sd,
# 						 data=mydata,control = Weka_control(M=2))

fit<-J48(formula = method.rates_s_1~.,data = mydata,control = Weka_control(M=20))

