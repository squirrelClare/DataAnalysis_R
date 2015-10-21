#获取表JL_ZHUANBIAN_LOAD表中的缺失值信�?




#根据条表的名字获取相应的SQL语句
get_sqls<-function(field_name){
  sql<-'create table ZHUANBIAN_COUNT_NA as select '
  sql<-paste(sql," ",field_name[1],",",field_name[4],",(")
  for(i in 5:length(field_name)){
    sql<-paste(sql,"case when ",field_name[i],"=0 then 1 else 0 end")
    if(i!=length(field_name))
      sql<-paste(sql,'+')
  }
  sql<-paste(sql,') as coutn_na from JL_ZHUANBIAN_LOAD;')
  return(sql)
}

library(RMySQL)

#连接数据�?
conn<-dbConnect(RMySQL::MySQL(),dbname='DM2015',username='dimer',host='192.168.10.87',
                port=3306,password='data123')
#获取表头信息
res<-dbSendQuery(conn,statement = 'show columns from JL_ZHUANBIAN_LOAD;')
field_name<-fetch(res,n = -1)$Field

#汇总信�?
sql<-get_sqls(field_name)
dbSendQuery(conn,statement = sql)
dbDisconnect(conn)