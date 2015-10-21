# 在数据库DM2015中执行sql，并返回查询的结果
holiday_exeDm2015<-function(sql) {
  library(RMySQL)
  conn<-dbConnect(RMySQL::MySQL(),dbname='DM2015',username='dimer',host='192.168.10.87',
                  port=3306,password='data123')
  res<-dbSendQuery(conn,statement=sql)
  mydata<-fetch(res,n=-1)
  dbDisconnect(conn)
  return(mydata)
}

sql<-"select * from JL_TAIQU_LOAD where STREET_WORKSTATION_NAME='福田' and LOAD_DATE='2013-12-17';"

src<-holiday_exeDm2015(sql)
loadData<-src[5:100]
loadData<-as.matrix.data.frame(loadData)
id<-src$VKONT
path<-paste0('~/Documents/20131217/')
if (!file.exists(path)){
   	dir.create(path)
}
setwd(path)
for(i in 1:nrow(loadData)){
 	tiff(file=paste0('',id[i],'.tiff'), res = 300, width = 2400, height = 2400, compression = "lzw")    
    	#画图
    
    	plot(loadData[i,],type='l',col = 'red',xaxt = 'n',xlim = c(0,100),ylim=rang,xlab = '时点',ylab = '负荷',main=paste0(id[i]))
    	dev.off()
  }