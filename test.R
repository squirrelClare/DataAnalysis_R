x<-c(1,2,3,4)

for(jobsrv in jobservers)
{
        #7005端口，监听主级任务分发
       paste("ssh ",jobsrv," 'source /etc/profile;gearmand -L ",jobsrv," -p 7005 -d -R'&",sep="")
        #7003端口，监听次级任务分发
        # paste("ssh ",jobsrv," 'source /etc/profile;gearmand -L ",jobsrv," -p 7003 -d -R'&",sep="")
}
paste("ssh ",jobservers," 'source /etc/profile;gearmand -L ",jobservers," -p 7005 -d -R'&",sep="")