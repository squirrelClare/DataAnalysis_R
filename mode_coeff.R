##############[[[使用偏最小二乘回归算法进行回归]]]-----------------------------------
	modExpr=paste("plsr(VALUE~",afmula,",data=traindata)",sep="")
	buildmodel(modExpr,traindata,"plsr")
	##############[[[使用多元自适应样条回归算法进行回归]]]-----------------------------
	modExpr=paste("earth(VALUE~",afmula,",data=traindata)",sep="")
	buildmodel(modExpr,traindata,"earth")
	##############[[[使用随机森林算法进行回归]]]--------------------------------------
	modExpr=paste("randomForest(VALUE~",afmula,",data=traindata,ntree=500)",sep
cador(游皓麟) 06-01 09:47:55
="")
	buildmodel(modExpr,traindata,"randomForest")
	##############[[[使用高斯过程回归进行回归]]]
	modExpr=paste("gausspr(VALUE~",afmula,",data=traindata,kernel='rbfdot',C=10,fit=F)",sep="")
	buildmodel(modExpr,traindata,"gausspr_rbfdot")
	modExpr=paste("gausspr(VALUE~",afmula,",data=traindata,kernel='polydot',C=10,fit=F)",sep="")
	buildmodel(modExpr,traindata,"gausspr_polydot")
	#modExpr=paste("gausspr(VALUE~",afmula,",data=traindata,kernel='besseldot',C
cador(游皓麟) 06-01 09:47:55
=10,fit=F)",sep="")
	#buildmodel(modExpr,traindata,"gausspr_besseldot")	
	##############[[[使用支持向量机进行回归]]]----------------------------------------
	modExpr=paste("ksvm(VALUE~",afmula,",data=traindata,kernel='rbfdot',C=20)",sep="")
	buildmodel(modExpr,traindata,"ksvm_rbfdot")
