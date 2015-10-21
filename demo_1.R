library(DMwR)

tmp_data<-mydata[c(1:10,20:30,70:80,100:120),5:ncol(mydata)]
tmp_matrix<-as.matrix(tmp_data)
tmp_matrix[which(tmp_matrix==0)]<-NA
tmp_data<-knnImputation(as.data.frame(tmp_matrix),k=10)
distances<-dist(tmp_data)
hc<-hclust(distances,'single')
plot(hc)
plot(hc,hang=-1,type="tirangle")
