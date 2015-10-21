#在数据库DM2015中执行sql，并返回查询的结果
exe_dm2015<-function(sql){
  library(RMySQL)
  conn<-dbConnect(RMySQL::MySQL(),dbname='DM2015',username='dimer',host='192.168.10.87',
                  port=3306,password='data123')

  res<-dbSendQuery(conn,statement=sql)
  mydata<-fetch(res,n=-1)
  dbDisconnect(conn)
  return(mydata)
}

#在数据库HUMBIRD中执行sql，并返回查询的结果
exe_humbird<-function(sql){
  library(RMySQL)
  conn<-dbConnect(RMySQL::MySQL(),dbname='HUMMINGBIRD',username='humbird',host='192.168.10.87',
                  port=3306,password='humbird123')
  res<-dbSendQuery(conn,statement=sql)
  mydata<-fetch(res,n=-1)
  dbDisconnect(conn)
  return(mydata)
}

#对原始负荷数据和预测负荷数据进行计算，返回百分误差率矩阵
get_error_mat<-function(data_src,data_pre,ahead,k){
  #将数据按台区编号进行划分
  srcs<-split(data_src,data_src$VKONT)
  pres<-split(data_pre,data_pre$location_id)
  
  #获取原始数据和预测数据的记录数目
  srcs_length<-as.numeric(sapply(X = srcs,FUN = nrow))
  pres_length<-as.numeric(sapply(X = pres,FUN = nrow))
  srcs<-srcs[srcs_length==ahead]
  pres<-pres[pres_length==ahead]
  
  
  #对原始数据和预测数据按台区编号和预测日期取交集
  tag<-pres_length==srcs_length
  pres_new<-pres[names(pres)%in%names(srcs)]
  srcs_new<-srcs
  
  #初始化一个存放误差率的空矩阵
  n<-length(pres_new)
  records<-matrix(nrow = n,ncol = ahead)
  
  #台区计算百分误差率
  for(i in 1:n){
    records[i,]<-as.numeric(per_error(srcs_new[[i]],pres_new[[i]],k))
  }
  rownames(records)<-names(pres_new)
  return(records)
}

#计算单个台区原始数据和预测数据的百分误差率
per_error<-function(src,pre,k){
  #预处理
  src<-src[order(src$LOAD_DATE),]
  pre<-pre[order(pre$forecast_date),]
  tmp_src<-as.matrix(src[5:100])
  tmp_pre<-as.matrix(pre[2:97])
  tmp_pre<-matrix(as.numeric(tmp_pre),nrow=nrow(tmp_pre),dimnames = list(rownames(tmp_pre),colnames(tmp_pre)))
  
  #误差率计算
  n<-nrow(tmp_src)
  means<-rep(x = 0,n)
  
  #每天误差率为96时点所有误差率的均值
  if(k==1){
    errors<-abs(tmp_pre-tmp_src)/tmp_src
    for(i in 1:n){
      tmp<-errors[i,]
      means[i]<-median(x = tmp[tmp<0.7])
    }
  }else{
  #每天误差率为96时点所有差值绝对值求和后与原始数据和的比值
    if(k==2){
      for(i in 1:n){
        means[i]<-sum(abs(tmp_pre[i,]-tmp_src[i,]))/sum(tmp_src[i,])
    }
    }
  }
  #   median_row<-apply(X = errors,MARGIN = 1,FUN = median)#对每一行的误差率取中位数
  return(means)
}

plt_id<-function(id,path='/tmp'){
  #获取最大日期和最小日期
  date_max<-exe_humbird('select max(forecast_date) from L4_FC_TQ_DETAIL_20140501_bak;')[[1]]
  date_min<-exe_humbird('select min(forecast_date) from L4_FC_TQ_DETAIL_20140501_bak;')[[1]]
  
  src<-exe_dm2015(paste0("SELECT * from JL_TAIQU_LOAD WHERE VKONT='",
                              id,"' and LOAD_DATE >='",date_min,
                              "' and LOAD_DATE <='",date_max,"';"))
  pre<-exe_humbird(paste0("select * from L4_FC_TQ_DETAIL_20140501_bak where location_id='",id,"';"))
  pre<-pre[order(pre$forecast_date),]
  src<-src[order(src$LOAD_DATE),]
  
  src_mat<-as.matrix(src[5:100])
  pre_mat<-as.matrix(pre[2:97])
  n<-nrow(pre_mat)
  forecast_dates<-pre$forecast_date
  weekdays<-weekdays(strptime(forecast_dates,format = '%Y-%m-%d'))
  path<-paste0('~/Documents/预测与原始数据_',id)
  if (!file.exists(path)){
    dir.create(path)
  }
  setwd(path)
  for(i in 1:n){
    tiff(file=paste0('',forecast_dates[i],'.tiff'), res = 300, width = 2400, height = 2400, compression = "lzw")    
    #画图
    
    top_bot<-as.numeric(c(range(pre_mat[i,]),range(src_mat[i,])))
    rang<-range(top_bot)+c(-50,50)
    
    plot(pre_mat[i,],type='l',col = 'red',xaxt = 'n',xlim = c(0,100),ylim=rang,xlab = '时点',ylab = '负荷',main=paste0(forecast_dates[i],'__',weekdays[i]))
    lines(src_mat[i,],type='l',col='blue')
    axis(side =1,at = seq(0,95,4),labels = colnames(pre)[2:97][seq(1,96,4)] )
    legend("bottomright",legend=c('预测','原始'),col=c('red','blue'),lty=1)
    dev.off()
  }
  
  # 提取预测预期前30天的数据
  date_start<-as.Date(date_min)-30
  date_end<-as.Date(date_min)-1
  src<-exe_dm2015(paste0("SELECT * from JL_TAIQU_LOAD WHERE VKONT='",
                         id,"' and LOAD_DATE >='",date_start,
                         "' and LOAD_DATE <='",date_end,"';"))
  src<-src[order(src$LOAD_DATE),]
  src_mat<-as.matrix(src[5:100])
  dates<-src$LOAD_DATE
  
  for(i in 1:30){
    tiff(file=paste0('',dates[i],'.tiff'), res = 300, width = 2400, height = 2400, compression = "lzw")    
    #画图
    
    rang<-range(src_mat[i,])+c(-50,50)
    
    plot(src_mat[i,],type='l',col = 'red',xaxt = 'n',xlim = c(0,100),ylim=rang,xlab = '时点',ylab = '负荷',main=paste0(dates[i]))
#     axis(side =1,at = seq(0,95,4),labels = colnames(pre)[2:97][seq(1,96,4)] )
#     legend("bottomright",legend=c('预测','原始'),col=c('red','blue'),lty=1)
    dev.off()
  }
}
#百分误差率统计
statist<-function(k){
  
  #获取预测数据的全部台区ID
  ids<-exe_humbird('select DISTINCT(location_id) from L4_FC_TQ_DETAIL_20140501_bak;')

  
  #将id数据写人DM2015数据库中
  conn<-dbConnect(RMySQL::MySQL(),dbname='DM2015',username='dimer',host='192.168.10.87',
                  port=3306,password='data123')
  dbWriteTable(conn,name = 'tmpid',ids)
  dbDisconnect(conn)
  
  #获取最大日期和最小日期
  date_max<-exe_humbird('select max(forecast_date) from L4_FC_TQ_DETAIL_20140501_bak;')[[1]]
  date_min<-exe_humbird('select min(forecast_date) from L4_FC_TQ_DETAIL_20140501_bak;')[[1]]
  
  ahead<-as.numeric(as.Date(date_max)-as.Date(date_min)+1)
  
  data_src<-exe_dm2015(paste0("SELECT * from JL_TAIQU_LOAD WHERE VKONT in (SELECT location_id from tmpid) and LOAD_DATE >='",date_min,"' and LOAD_DATE <='",date_max,"';"))
  data_pre<-exe_humbird('select * from L4_FC_TQ_DETAIL_20140501_bak;')
  
  conn<-dbConnect(RMySQL::MySQL(),dbname='DM2015',username='dimer',host='192.168.10.87',
                  port=3306,password='data123')
  dbSendQuery(conn,statement = "drop table tmpid;")
  dbDisconnect(conn)
  
  return(get_error_mat(data_src,data_pre,ahead,k))
}

#对原始负荷数据和预测负荷数据进行计算，返回每天最大负荷点的误差率
get_max_point_errors<-function(data_src,data_pre){
  #将数据按台区编号进行划分
  srcs<-split(data_src,data_src$VKONT)
  pres<-split(data_pre,data_pre$location_id)
  
  #获取原始数据和预测数据的记录数目
  srcs_length<-as.numeric(sapply(X = srcs,FUN = nrow))
  pres_length<-as.numeric(sapply(X = pres,FUN = nrow))
  
  #对原始数据和预测数据按台区编号和预测日期取交集
  tag<-pres_length==srcs_length
  pres_new<-pres[tag]
  srcs_new<-srcs[tag]
  
  #初始化一个存放误差率的空矩阵
  n<-length(pres_new)
  records<-matrix(nrow = n,ncol = 7)
  
  #台区计算百分误差率
  for(i in 1:n){
    records[i,]<-as.numeric(per_taiqu_rror(srcs_new[[i]],pres_new[[i]]))
  }
  rownames(records)<-names(pres_new)
  return(records)
}

#计算单个台区原始数据和预测数据的百分误差率
per_taiqu_rror<-function(src,pre){
  #预处理
  src<-src[order(src$LOAD_DATE),]
  per<-pre[order(pre$forecast_date),]
  tmp_src<-as.matrix(src[5:100])
  tmp_pre<-as.matrix(pre[2:97])
  
  #定位原始数据每天的最大负荷位置
  locations<-which(tmp_src==apply(tmp_src,MARGIN = 1,max),arr.ind = TRUE)
  #误差率计算
  n<-nrow(tmp_src)
  erros<-rep(0,n)
  for(i in 1:n){
    erros[i]<-abs(tmp_pre[locations[i,1],locations[i,2]]-tmp_src[locations[i,1],locations[i,2]])/tmp_src[locations[i,1],locations[i,2]]
  }
  #   median_row<-apply(X = errors,MARGIN = 1,FUN = median)#对每一行的误差率取中位数
  return(erros)
}

#每天最高负荷百分误差率统计
max_point_errors<-function(){
  
  #获取预测数据的全部台区ID
  ids<-exe_humbird('select DISTINCT(location_id) from L4_FC_TQ_DETAIL_20140501_bak;')
  
  
  #将id数据写人DM2015数据库中
  conn<-dbConnect(RMySQL::MySQL(),dbname='DM2015',username='dimer',host='192.168.10.87',
                  port=3306,password='data123')
  dbWriteTable(conn,name = 'tmpid',ids)
  dbDisconnect(conn)
  
  #获取最大日期和最小日期
  date_max<-exe_humbird('select max(forecast_date) from L4_FC_TQ_DETAIL_20140501_bak;')[[1]]
  date_min<-exe_humbird('select min(forecast_date) from L4_FC_TQ_DETAIL_20140501_bak;')[[1]]
  
  data_src<-exe_dm2015(paste0("SELECT * from JL_TAIQU_LOAD WHERE VKONT in (SELECT location_id from tmpid) and LOAD_DATE >='",date_min,"' and LOAD_DATE <='",date_max,"';"))
  data_pre<-exe_humbird('select * from L4_FC_TQ_DETAIL_20140501_bak;')
  
  conn<-dbConnect(RMySQL::MySQL(),dbname='DM2015',username='dimer',host='192.168.10.87',
                  port=3306,password='data123')
  dbSendQuery(conn,statement = "drop table tmpid;")
  dbDisconnect(conn)
  
  return(get_max_point_errors(data_src,data_pre))
}


# d<-density(x = m[,2])
# plot(d)
# polygon(d,border = 'green',col = 'blue')
# abline(v = c(.3,.5),col='red')


plt_error<-function(){
  
  path<-paste0('~/Documents/预测误差统计')
  if (!file.exists(path)){
    dir.create(path)
  }
    
  errors<-statist()
  setwd(path)
  
  for(i in 1:7){
    if(i%%4==1){
      tiff(file=paste0('第',i,'天预测误差率分布','.tiff'), res = 300, width = 2400, height = 2400, compression = "lzw")    
      par(mfrow = c(2,2))
    }
    #画图
    
    rang<-c(0,600)
    f<-cut(as.numeric(errors[,i]),breaks = 0.01*c(0,5,7,10,100))
    barplot(height = table(f),col = c("lightblue", "mistyrose", "lightcyan","lavender"),border = 'red',xlab = '误差率范围',ylab = '频数',
            main = paste0('第',i,'天预测误差率分布'),ylim=rang)
    if(i%%4==0){
      dev.off()
    }
  }
  dev.off()
}

main<-function(k){
  m<-statist(k)
  nr<-nrow(m)
  nc<-ncol(m)
  
  records<-matrix(nrow = 4,ncol = 6)
  colnames(records)<-c('(0,5%]','(5%,7%]','(7%,10%]','(10%,15%]','(15%,20%]','(20%,100%]')
  for( i in 1:ncol(m)){
    f<-cut(as.numeric(m[,i]),breaks = 0.01*c(0,5,7,10,15,20,100))
    records[i,]<-as.numeric(table(f)/nr)
  }
  
  f<-cut(as.numeric(m),breaks = 0.01*c(0,5,7,10,15,20,100))
  records[4,]<-as.numeric(table(f)/(nr*nc))
  rownames(records)<-c('第一天','第二天','第三天','综合')
  write.csv(records,paste0("~/Desktop/节假日百分误差率统计k=",k,".csv"))
}
