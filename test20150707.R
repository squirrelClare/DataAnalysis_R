x <- methChoice_exeDm2015("select * from JL_TAIQU_LOAD where VKONT='000047296860' and LOAD_DATE='2014-05-07'")
y <- methChoice_exeDm2015("select id from loadModule120140702_preds")[[1]]
z<- methChoice_exeDm2015("select id from extendOut20140702_preds")[[1]]
x <- unlist(x[5:100])
y <- unlist(y[2:97])
