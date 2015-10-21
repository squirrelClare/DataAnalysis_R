#���ܣ�
#     ��ȡ��summary_zhuanbian_count_na�е�ȫ��ר����
#������
#     conn��MySQL���ݿ��һ������
#���أ�
#     VKONTS:��summary_zhuanbian_count_na�е�ȫ��ר����
get_VKONTS<-function(conn){
  sql="select distinct VKONT from summary_zhuanbian_count_na;"
  VKONTS<-fetch(dbSendQuery(conn,statement=sql),n=-1)
  return(unlist(VKONTS))
}


#���ܣ�
#     ��ȡ��summary_zhuanbian_count_na�еı��ΪVKONT��ר����·ݵ�ȫ��ȴʧȥ���
#������
#     conn��MySQL���ݿ��һ������
#     VKONT:ר����
#���أ�
#     total_nas:ר���ΪVKONT��ȫ��ȱʧ��Ϣ
get_total_na<-function(conn,VKONT){
  sql=paste0("select miss from summary_zhuanbian_count_na where VKONT=",VKONT,' ORDER BY year,month;')
  total_nas<-fetch(dbSendQuery(conn,statement=sql),n=-1)
  return(unlist(total_nas))
}

#���ܣ�
#     ���zhuanbian_summary�м���ı��ΪVKONT��ר����·ݵ�ȱʧȥ���
#������
#     conn��MySQL���ݿ��һ������
#     VKONT:ר����
#���أ�
#     �޷���ֵ
insert_data<-function(conn,VKONT){
  total_nas<-get_total_na(conn,VKONT)
  tmp_join<-paste0(total_nas,collapse=',')
  print(tmp_join)
  
  sql=paste0("INSERT INTO zhuanbian_summary VALUES(\'",VKONT,'\',',tmp_join,');')
  print(sql)
  
  dbSendQuery(conn,statement=sql)
}


#���ܣ�
#     �жϱ�summary_zhuanbian_count_na�б��ΪVKONT��ר�������Ƿ�����
#������
#     conn��MySQL���ݿ��һ������
#     VKONT:ר����
#���أ�
#     ������а���VKONT��13���µ����ݷ���T�����򷵻�F
is_complete<-function(coon,VKONT){
  sql=paste0('SELECT COUNT(*) from summary_zhuanbian_count_na WHERE VKONT=\'',VKONT,'\';')
  n<-unlist(fetch(dbSendQuery(conn,statement=sql),n=-1))
  
  if(n==13){
    return(T)
  }else {
    return(F)
  }
}

#���ܣ�
#     ���summary_zhuanbian_count_na�����ı��ΪVKONT��ר����·ݵ�ȱʧ��¼
#������
#     conn��MySQL���ݿ��һ������
#     VKONT:ר����
#���أ�
#     �޷���ֵ
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


#���ܣ�
#     ���summary_zhuanbian_count_na�����ĸ�ר����·ݵ�ȱʧ��¼
#������
#     conn��MySQL���ݿ��һ������
#     VKONT:ר����
#���أ�
#     �޷���ֵ

fill_summary_zhuanbian_count_na<-function(conn){
  
  sql="SELECT VKONT from (SELECT VKONT ,COUNT(*) as n from summary_zhuanbian_count_na GROUP BY VKONT) as tmp_table WHERE n!=13;"
  VKONTS<-unlist(fetch(dbSendQuery(conn,statement=sql),n=-1))#��ȡר���ż�
  lapply(X=VKONTS,FUN=fill_VKONT,conn=conn)
}


library(RMySQL)
conn<-dbConnect(RMySQL::MySQL(),dbname='DM2015',username='dimer',host='192.168.10.87',
                port=3306,password='data123')
#fill_summary_zhuanbian_count_na(conn)#����summary_zhuanbian_count_naȱʧ����
VKONTS<-get_VKONTS(conn=conn)
#print(length(VKONTS))

# for(i in 1:length(VKONTS)){
#   insert_data(conn=conn,VKONT=VKONTS[i])
# }
#

lapply(X=VKONTS[2:length(VKONTS)],FUN=insert_data,conn=conn)
# fill_VKONT(conn,VKONT="000010000084")
dbDisconnect(conn)