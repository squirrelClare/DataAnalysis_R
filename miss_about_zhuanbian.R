#���ܣ�
#     ��ȡ��zhuanbian_summary�е�ȫ������
#������
#     ��
#���أ�
#     mydata:��ȡ��������
get_data<-function(){
  library(RMySQL)
  conn<-dbConnect(RMySQL::MySQL(),dbname='DM2015',username='dimer',host='192.168.10.87',
                  port=3306,password='data123')
  
  sql<-"SELECT * from zhuanbian_summary;"
  res<-dbSendQuery(conn,statement=sql)
  mydata<-fetch(res,n=-1)
  dbDisconnect(conn)
  return(mydata)
}

library(dfexplore)
mydata<-get_data()
newdata<-mydata
tag<-(mydata==1)
mydata[tag]<-NA
mydata$sum<-apply(newdata[2:14],MARGIN=1,FUN=sum)

newdata$sum<-mydata$sum
newdata<-newdata[order(newdata$sum),]
mydata<-mydata[order(mydata$sum),]

sum_v<-newdata$sum
n_mis<-sum(sum_v!=0)
n_perf<-sum(sum_v==0)
# dfplot(dfdescription=mydata,title='ר��ȱʧ���ͼ')

sum_miss<-function(i){
  return(sum(sum_v==i))
}

n_values<-unlist(lapply(1:max(sum_v),FUN=sum_miss))
options(digits=4)
names(n_values)<-paste(1:13,'����',round(n_values/n_mis,digits=4)*100,'%')
pie(n_values)