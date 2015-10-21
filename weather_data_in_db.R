data_format<-function(file_name){
  data<-read.csv(paste0('~/Desktop/weth_data/',file_name))
  addr<-unlist(strsplit(unlist(strsplit(file_name,split = '\\.'))[1],split='\\_'))
  area<-addr[1]
  street<-addr[2]
  data$area<-rep(area,nrow(data))
  data$street<-rep(street,nrow(data))
  return(data)
}

files<-list.files('~/Desktop/weth_data/')
datas<-lapply(files,FUN = data_format)

tmp<-data.frame()
for(i in 1:39){
  tmp<-rbind(tmp,datas[[i]])
}

write.csv(tmp,'~/Desktop/weather_data.csv')

library(RMySQL)
conn<-dbConnect(RMySQL::MySQL(),dbname='DM2015',username='dimer',host='192.168.10.87',
                port=3306,password='data123')
# dbSendQuery(conn,statement="set names 'utf8';")
dbWriteTable(conn,name = 'weather_data',value = tmp)
dbDisconnect(conn)

library(RMySQL)
conn<-dbConnect(RMySQL::MySQL(),dbname='DM2015',username='dimer',host='192.168.10.87',
                port=3306,password='data123')
dbSendQuery(conn,statement="set names 'utf8';")
res<-dbSendQuery(conn,statement="select * from weather_data where street='西丽';")
d<-fetch(res,n = -1)
dbDisconnect(conn)

