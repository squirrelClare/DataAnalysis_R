
check_date<-function(file_path)
{
  	weather_data<-read.csv(file_path)
  	index_X<-paste0(rep('X',7),1:7)
  	index_R<-paste0(rep('R',7),1:7)
  	index_C<-paste0(rep('C',7),1:7)
  	index<-c('WETH_DATE',index_X,index_R,index_C)
  	src_data<-weather_data[index]
	n_row<-nrow(src_data)

	tag<-c()
	for (i in 1:n_row)
	{
		tmp<-unlist(src_data[i,])
		tmp<-strptime(tmp,format = '%Y-%m-%d')
		tag[i]=(max(tmp)==tmp[1])
	}
	return(length(which(tag==FALSE)))
}
is_equal<-function(date_db,coef_db,coef_VAL,p_index){
	tmp_mat<-matrix(nrow = nrow(coef_VAL),ncol = ncol(coef_VAL))
	 rownames(tmp_mat)<-rownames(coef_VAL)
	 colnames(tmp_mat)<-colnames(coef_VAL)
	 for (e in rownames(coef_VAL)) 
	 {
# 	   print(coef_VAL[e,])
	 	tmp_mat[e,]<-unlist(lapply(date_db[e,],FUN=function(date){return(coef_db[date,p_index])}))
#      print(tmp_mat[e,])
	 }

	 n_false=length(which((tmp_mat==coef_VAL)==FALSE))
	 if (n_false==0)
	 {
	 	return(TRUE)
	 }else
	 {
	 	return(FALSE)
	 }
}


is_unique<-function(coef_VAL)
{
	tag<-lapply(1:nrow(coef_VAL),FUN=function(i)
	{
		return(length(coef_VAL[i,])==length(unique(coef_VAL[i,])))
	})
	return(length(which(tag==FALSE))==0)
}


unique_check<-function(dir_path)
{
	setwd(dir_path)
	index_X<-paste0(rep('X',7),1:7,'_VAL')
  	index_R<-paste0(rep('R',7),1:7,'_VAL')
  	index<-c('VALUE',index_X,index_R)

  	    #载入并检测小波系数D1
  	load('C_D1datalist.rd')
  	for (i in 1:96) 
  	{
  		tmp<-(datalist[[i]])[index]
  		rownames(tmp)<-(datalist[[i]])[[1]]
  		D1_VAL<-as.matrix(tmp)

  		tag=is_unique(D1_VAL)
    	print(paste0('小波系数D1中第',i,'个时点：  ',tag))

  	}

    
  	load('C_D2datalist.rd')
  	for (i in 1:96) 
  	{
  	  tmp<-(datalist[[i]])[index]
  	  rownames(tmp)<-(datalist[[i]])[[1]]
  	  D2_VAL<-as.matrix(tmp)
  	  
  	  tag=is_unique(D2_VAL)
  	  print(paste0('小波系数D2中第',i,'个时点：  ',tag))
  	        
  	}
    
  	load('C_D3datalist.rd')
  	for (i in 1:96) 
  	{
  	  tmp<-(datalist[[i]])[index]
  	  rownames(tmp)<-(datalist[[i]])[[1]]
  	  D3_VAL<-as.matrix(tmp)
  	  
  	  tag=is_unique(D3_VAL)
  	  print(paste0('小波系数D3中第',i,'个时点：  ',tag))
  	        
  	}
    
  	load('C_D4datalist.rd')
  	for (i in 1:96) 
  	{
  	  tmp<-(datalist[[i]])[index]
  	  rownames(tmp)<-(datalist[[i]])[[1]]
  	  D4_VAL<-as.matrix(tmp)
  	  
  	  tag=is_unique(D4_VAL)
  	  print(paste0('小波系数D4中第',i,'个时点：  ',tag))
  	        
  	}
    
  	load('C_S4datalist.rd')
  	for (i in 1:96) 
  	{
  	  tmp<-(datalist[[i]])[index]
  	  rownames(tmp)<-(datalist[[i]])[[1]]
  	  S4_VAL<-as.matrix(tmp)
  	  
  	  tag=is_unique(S4_VAL)
  	  print(paste0('小波系数S4中第',i,'个时点：  ',tag))
  	        
  	}
}


check_data<-function(dir_path){
    setwd(dir_path)
    #读取数据
  	date_data<-read.csv('天气数据.csv')
    
  	index_X<-paste0(rep('X',7),1:7)
  	index_R<-paste0(rep('R',7),1:7)
    
    #日期对照表
    date_db<-as.matrix(date_data[c(index_X,index_R)])
    row.names(date_db)<-date_data$WETH_DATE
    load('loadcurves.rd')
    
    #提取小波高频系数1
    tmp<-(loadcurves[[1]])[2:97]
    rownames(tmp)<-(loadcurves[[1]])[[1]]
  	D1_db<-as.matrix(tmp)
    
  	#提取小波高频系数2
  	tmp<-(loadcurves[[2]])[2:97]
  	rownames(tmp)<-(loadcurves[[2]])[[1]]
  	D2_db<-as.matrix(tmp)
    
  	#提取小波高频系数3
  	tmp<-(loadcurves[[3]])[2:97]
  	rownames(tmp)<-(loadcurves[[3]])[[1]]
  	D3_db<-as.matrix(tmp)
    
  	#提取小波高频系数4
  	tmp<-(loadcurves[[4]])[2:97]
  	rownames(tmp)<-(loadcurves[[4]])[[1]]
  	D4_db<-as.matrix(tmp)
    
  	#提取小波低频系数
  	tmp<-(loadcurves[[5]])[2:97]
  	rownames(tmp)<-(loadcurves[[5]])[[1]]
  	S4_db<-as.matrix(tmp)

  	index_X<-paste0(index_X,'_VAL')
  	index_R<-paste0(index_R,'_VAL')

    #载入并检测小波系数D1
  	load('C_D1datalist.rd')
  	for (i in 1:96) 
  	{
  		tmp<-(datalist[[i]])[c(index_X,index_R)]
  		rownames(tmp)<-(datalist[[i]])[[1]]
  		D1_VAL<-as.matrix(tmp)

  		tag=is_equal(date_db = date_db,coef_db=D1_db,coef_VAL=D1_VAL,i)
    	print(paste0('小波系数D1中第',i,'个时点：  ',tag))

  	}

    
  	load('C_D2datalist.rd')
  	for (i in 1:96) 
  	{
  	  tmp<-(datalist[[i]])[c(index_X,index_R)]
  	  rownames(tmp)<-(datalist[[i]])[[1]]
  	  D2_VAL<-as.matrix(tmp)
  	  
  	  tag=is_equal(date_db = date_db,coef_db=D2_db,coef_VAL=D2_VAL,i)
  	  print(paste0('小波系数D2中第',i,'个时点：  ',tag))
  	        
  	}
    
  	load('C_D3datalist.rd')
  	for (i in 1:96) 
  	{
  	  tmp<-(datalist[[i]])[c(index_X,index_R)]
  	  rownames(tmp)<-(datalist[[i]])[[1]]
  	  D3_VAL<-as.matrix(tmp)
  	  
  	  tag=is_equal(date_db = date_db,coef_db=D3_db,coef_VAL=D3_VAL,i)
  	  print(paste0('小波系数D3中第',i,'个时点：  ',tag))
  	        
  	}
    
  	load('C_D4datalist.rd')
  	for (i in 1:96) 
  	{
  	  tmp<-(datalist[[i]])[c(index_X,index_R)]
  	  rownames(tmp)<-(datalist[[i]])[[1]]
  	  D4_VAL<-as.matrix(tmp)
  	  
  	  tag=is_equal(date_db = date_db,coef_db=D4_db,coef_VAL=D4_VAL,i)
  	  print(paste0('小波系数D4中第',i,'个时点：  ',tag))
  	        
  	}
    
  	load('C_S4datalist.rd')
  	for (i in 1:96) 
  	{
  	  tmp<-(datalist[[i]])[c(index_X,index_R)]
  	  rownames(tmp)<-(datalist[[i]])[[1]]
  	  S4_VAL<-as.matrix(tmp)
  	  
  	  tag=is_equal(date_db = date_db,coef_db=S4_db,coef_VAL=S4_VAL,i)
  	  print(paste0('小波系数S4中第',i,'个时点：  ',tag))
  	        
  	}
   #  tag=is_equal(date_db = date_db,coef_db=D1_db,coef_VAL=D1_VAL,1)
   #  print(tag)
}



n_false<-check_date('~/Documents/data_check/天气数据.csv')
