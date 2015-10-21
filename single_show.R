library(RMySQL)

conn<-dbConnect(RMySQL::MySQL(),dbname='DM2015',username='dimer',host='192.168.10.87',
                port=3306,password='data123')
res<-dbSendQuery(conn,statement = 'select * from tmp_000010000125;')
mydata<-fetch(res,n = -1)
dbDisconnect(conn)

plot(x=1:96,y=mydata[2,5:100],type='l',col='red')
#for(i in 2:10)
#  lines(x=1:96,y=mydata[i,5:100],col='red')
#for(i in 11:50)
#  lines(x=1:96,y=mydata[i,5:100],col='green')
#for(i in 51:80)
#  lines(x=1:96,y=mydata[i,5:100],col='black')

lines(x=1:96,y=mydata[3,5:100],col='yellow')
lines(x=1:96,y=mydata[4,5:100],col='black')
lines(x=1:96,y=mydata[7,5:100],col='green')
lines(x=1:96,y=mydata[8,5:100],col='green')



i=7*0
plot(x=1:96,y=mydata[2,5:100],type='l',col='red')
lines(x=1:96,y=mydata[3+i,5:100],col='yellow')
lines(x=1:96,y=mydata[4+i,5:100],col='black')
lines(x=1:96,y=mydata[7+i,5:100],col='green')
lines(x=1:96,y=mydata[8+i,5:100],col='green')


