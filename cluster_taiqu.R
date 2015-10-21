#��ذ�����
library(amap)
library(rattle)
library(fpc)


#���ܣ�
#     ��ȡ��ʽ���������
#������
#     path:csv��ʽ���ļ�����·��
#���أ�
#     mydata:��ʽ��������ݿ�
data_format<-function(path){
  src_data<-read.csv(path)
  tmp<-src_data[,3:98]
  mydata<-as.data.frame(t(scale(t(tmp))))#���ݱ�׼��
  LOAD_DATE<-strptime(src_data$LOAD_DATE,format='%Y/%m/%d')
  mydata$tag<-weekdays(LOAD_DATE)
  rownames(mydata)<-src_data$LOAD_DATE
  return(mydata)
}

#���ܣ�
#     �Ծ�������ͼ��ͼĬ��ͼƬ���·��D:\\test
#������
#     groups:��������
#     path:ͼ��洢·��
#���أ�
#     �޷���ֵ
pltcluster<-function(groups,path){ 
  setwd(path)
#   tiff(file='��Ϊ9��.tiff', res = 300, width = 2400, height = 2400, compression = "lzw")
#   par(mfrow = c(3,3))
  for(i in 1:length(groups)){
    tiff(file=paste0('���',i,'.tiff'), res = 300, width = 2400, height = 2400, compression = "lzw")
    
    tmp<-as.data.frame(groups[i])
    m=ncol(tmp)
    n=nrow(tmp)
    tmp<-tmp[,-m]
    colors<-sample(colors(),size = n)
    
    #��ͼ
    plot(unlist(tmp[1,]),type ="l",col = colors[1],ylim = c(-3,3),main=paste0('���',i),xlab='ʱ��',ylab='��ֵ')
    for(j in 2:n){
      lines(unlist(tmp[j,]),col=colors[j])
    }
    dev.off()
  }
#   dev.off()
}

#���ܣ�
#     ��ָ��̨�����ݽ��о������
#������
#     path:��������·��
#     k:��������Ĭ��Ϊ9
#���أ�
#     �޷���ֵ
data_manage<-function(path,k=9){
  #��ȡ��׼���������
  mydata<-data_format(path)
  data_cluster<-mydata[,-97]#ȥ��������
  
  #����
  chcluster<-hclusterpar(x=data_cluster,link='ward')#ŷ�Ͼ��룬����ƽ���;���
  # plot(chcluster)
  centers<-centers.hclust(x=data_cluster,h=chcluster,nclust=k)#��������
  groups = split(mydata,f = cutree(chcluster,k))#���ݷ���
  pltcluster(groups,"D:\\test")#����������ͼ��
  
  #ͳ�ƾ���ֲ����
  class_id<-cutree(chcluster,k)
  data_div<-data.frame(class_id,mydata$tag)#��������ķֲ�ͼ
  class_table<-table(data_div)
  write.csv(x = class_table,file = 'D:\\test\\table.csv')
  
  biases<-bias(groups,96)
  print(biases)
}

#���ܣ�
#     �Ծ�������ͳ��ƫ�������ŷʽ�������һ����¼�е�ĸ���
#������
#     path:��������·��
#     k:��ĸ���
#���أ�
#     �޷���ֵ

bias<-function(groups,n_point){
  biases<-lapply(groups,dist)
  summarys<-lapply(biases,summary)
  summarys<-matrix(unlist(summarys),ncol = 6,byrow = TRUE)
  summarys<-as.data.frame(summarys)/n_point
  names(summarys)<-c('Min.','1st Qu.','Median','Mean 3rd','Qu.','Max.')
  return(summarys)
}

data_manage('C:\\Users\\dell\\Desktop\\000047021626.csv',120)