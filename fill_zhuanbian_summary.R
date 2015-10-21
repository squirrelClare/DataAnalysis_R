#功能：
#     获取表summary_zhuanbian_count_na中的全部专变编号
#参数：
#     conn：MySQL数据库的一个连接
#返回：
#     VKONTS:表summary_zhuanbian_count_na中的全部专变编号
get_VKONTS<-function(conn){
  sql="select distinct VKONT from summary_zhuanbian_count_na;"
  VKONTS<-fetch(dbSendQuery(conn,statement=sql),n=-1)
  return(unlist(VKONTS))
}


#功能：
#     获取表summary_zhuanbian_count_na中的编号为VKONT的专变各月份的全部却失去情况
#参数：
#     conn：MySQL数据库的一个连接
#     VKONT:专变编号
#返回：
#     total_nas:专变号为VKONT的全部缺失信息
get_total_na<-function(conn,VKONT){
  sql=paste0("select miss from summary_zhuanbian_count_na where VKONT=",VKONT,' ORDER BY year,month;')
  total_nas<-fetch(dbSendQuery(conn,statement=sql),n=-1)
  return(unlist(total_nas))
}

#功能：
#     向表zhuanbian_summary中加入的编号为VKONT的专变各月份的缺失去情况
#参数：
#     conn：MySQL数据库的一个连接
#     VKONT:专变编号
#返回：
#     无返回值
insert_data<-function(conn,VKONT){
  total_nas<-get_total_na(conn,VKONT)
  tmp_join<-paste0(total_nas,collapse=',')
  print(tmp_join)
  
  sql=paste0("INSERT INTO zhuanbian_summary VALUES(\'",VKONT,'\',',tmp_join,');')
  print(sql)
  
  dbSendQuery(conn,statement=sql)
}


#功能：
#     判断表summary_zhuanbian_count_na中编号为VKONT的专变数据是否完整
#参数：
#     conn：MySQL数据库的一个连接
#     VKONT:专变编号
#返回：
#     如果表中包含VKONT的13个月的数据返回T，否则返回F
is_complete<-function(coon,VKONT){
  sql=paste0('SELECT COUNT(*) from summary_zhuanbian_count_na WHERE VKONT=\'',VKONT,'\';')
  n<-unlist(fetch(dbSendQuery(conn,statement=sql),n=-1))
  
  if(n==13){
    return(T)
  }else {
    return(F)
  }
}

#功能：
#     向表summary_zhuanbian_count_na中填充的编号为VKONT的专变各月份的缺失记录
#参数：
#     conn：MySQL数据库的一个连接
#     VKONT:专变编号
#返回：
#     无返回值
fill_VKONT<-function(conn,VKONT){
  
  if(is_complete(coon,VKONT)==F){  
    year<-c(rep('2013',4),rep('2014',9))
    month<-c(9:12,1:9)
    
    count_sqls<-paste0('SELECT COUNT(*) from summary_zhuanbian_count_na WHERE VKONT=\'',VKONT,'\' and year=\'',year,'\'and month=\'',month,'\';')
    insert_sqls<-paste0('INSERT into summary_zhuanbian_count_na VALUES(\'',VKONT,'\',\'',year,'\',\'',month,'\',2880,30);')
    for(i in 1:13){
      n<-unlist(fetch(dbSendQuery(conn,statement=count_sqls[i]),n=-1))
      if(n==0){
        dbSendQuery(conn,statement=insert_sqls[i])
      }
    }
  }
}


#功能：
#     向表summary_zhuanbian_count_na中填充的各专变各月份的缺失记录
#参数：
#     conn：MySQL数据库的一个连接
#     VKONT:专变编号
#返回：
#     无返回值

fill_summary_zhuanbian_count_na<-function(conn){
  
  sql="SELECT VKONT from (SELECT VKONT ,COUNT(*) as n from summary_zhuanbian_count_na GROUP BY VKONT) as tmp_table WHERE n!=13;"
  VKONTS<-unlist(fetch(dbSendQuery(conn,statement=sql),n=-1))#获取专变编号集
  lapply(X=VKONTS,FUN=fill_VKONT,conn=conn)
}


library(RMySQL)
conn<-dbConnect(RMySQL::MySQL(),dbname='DM2015',username='dimer',host='192.168.10.87',
                port=3306,password='data123')
#fill_summary_zhuanbian_count_na(conn)#填充表summary_zhuanbian_count_na缺失数据
VKONTS<-get_VKONTS(conn=conn)
#print(length(VKONTS))

# for(i in 1:length(VKONTS)){
#   insert_data(conn=conn,VKONT=VKONTS[i])
# }
#

lapply(X=VKONTS[2:length(VKONTS)],FUN=insert_data,conn=conn)
# fill_VKONT(conn,VKONT="000010000084")
dbDisconnect(conn)
