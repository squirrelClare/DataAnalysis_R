#构造创建表zhuanbian_summary的sql语句
x<-c(paste0('2013_',9:12),paste0('2104_',1:9))
y<-paste0(x,' int')
tmp<-y[1]
for(i in 2:(length(y)-1)){
  tmp<-paste0(tmp,',',y[i])
}
tmp<-paste0(tmp,',',y[length(y)])
sql<-paste0('create table zhuanbian_summary(VKONT varchar(50),',tmp,',primary key(VKONT));')

library(RMySQL)
conn<-dbConnect(RMySQL::MySQL(),dbname='DM2015',username='dimer',host='192.168.10.87',
                port=3306,password='data123')
dbSendQuery(conn,statement=sql)
dbDisconnect(conn)