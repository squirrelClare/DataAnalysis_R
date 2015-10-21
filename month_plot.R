myplot<-function(start,mydata){
  setwd("C:\\Users\\dell\\Desktop\\test_1")
  tiff(file=paste(mydata[start,4],' to ',mydata[start+27,4],'.tiff'), res = 300, width = 2400, height = 2400, compression = "lzw")
  par(mfrow=c(2,2))
  for(index in 0:3){
    i=index*7+start
#     plot(x=1:96,y=mydata[i,5:100],type='b',col='red',main=paste(mydata[2+i,4],' to ',mydata[8+i,4]))
#     lines(x=1:96,y=mydata[1+i,5:100],col='yellow',type='b')
#     lines(x=1:96,y=mydata[2+i,5:100],col='black',type='b')
#     lines(x=1:96,y=mydata[3+i,5:100],col='blue',type='b')
#     lines(x=1:96,y=mydata[4+i,5:100],col='pink',type='b')
#     lines(x=1:96,y=mydata[5+i,5:100],col='green',type='b')
#     lines(x=1:96,y=mydata[6+i,5:100],col='green',type='b')
    plot(x=1:96,y=mydata[i+5,5:100],type='b',col='red',main=paste(mydata[5+i,4],' to ',mydata[6+i,4]))
    lines(x=1:96,y=mydata[6+i,5:100],col='green',type='b')
  }
  dev.off()
}

# 在数据库DM2015中执行sql，并返回查询的结果
holiday_exeDm2015<-function(sql) {
  library(RMySQL)
  conn<-dbConnect(RMySQL::MySQL(),dbname='DM2015',username='dimer',host='192.168.10.87',
                  port=3306,password='data123')
  dbSendQuery(conn,statement="set names 'utf8';")
  res<-dbSendQuery(conn,statement=sql)
  mydata<-fetch(res,n=-1)
  dbDisconnect(conn)
  return(mydata)
}

library(DMwR)
sqlSrc<-paste0("select * from JL_TAIQU_LOAD where VKONT='000047001668'")
mydata<-holiday_exeDm2015(sqlSrc)


mydata<-knnImputation(data=mydata[5:ncol(mydata)],k=10)
replace

for (i in 0:13){
  myplot(2+i*28,mydata)
}