function (d, method = "complete", members = NULL) 
{
    #获取输入的方法
    METHODS <- c("ward.D", "single", "complete", "average", "mcquitty", 
        "median", "centroid", "ward.D2")
    if (method == "ward") {
        message("The \"ward\" method has been renamed to \"ward.D\"; note new \"ward.D2\"")
        method <- "ward.D"
    }

    #获取输入方法在METHODS中的位置
    i.meth <- pmatch(method, METHODS)
    if (is.na(i.meth)) 
        stop("invalid clustering method", paste("", method))
    if (i.meth == -1) 
        stop("ambiguous clustering method", paste("", method))
    n <- as.integer(attr(d, "Size"))
    if (is.null(n)) 
        stop("invalid dissimilarities")
    if (is.na(n) || n > 65536L) 
        stop("size cannot be NA nor exceed 65536")
    if (n < 2) 
        stop("must have n >= 2 objects to cluster")
    len <- as.integer(n * (n - 1)/2)
    if (length(d) != len) 
        (if (length(d) < len) 
            stop
        else warning)("dissimilarities of improper length")
    if (is.null(members)) 
        members <- rep(1, n)
    else if (length(members) != n) 
        stop("invalid length of members")
    storage.mode(d) <- "double"#设置数据框d的类型为double
    hcl <- .Fortran(C_hclust, n = n, len = len, method = as.integer(i.meth), 
        ia = integer(n), ib = integer(n), crit = double(n), members = as.double(members), 
        nn = integer(n), disnn = double(n), flag = logical(n), 
        diss = d)i
    hcass <- .Fortran(C_hcass2, n = n, ia = hcl$ia, ib = hcl$ib, 
        order = integer(n), iia = integer(n), iib = integer(n))
    structure(list(merge = cbind(hcass$iia[1L:(n - 1)], hcass$iib[1L:(n - 
        1)]), height = hcl$crit[1L:(n - 1)], order = hcass$order, 
        labels = attr(d, "Labels"), method = METHODS[i.meth], 
        call = match.call(), dist.method = attr(d, "method")), 
        class = "hclust")
}
<bytecode: 0x0000000008aef978>
<environment: namespace:stats>