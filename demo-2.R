rand_str<-function(len){
  chars<-'AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz0123456789'
  chars<-unlist(strsplit(chars,''))
  return(paste0(sample(chars,len),collapse=''))
}
paste("ssh hadoop@192.168.10.209 "," 'source /etc/profile;gearmand -L 192.168.10.209 "," -p 7005 --libpq-conninfo 'hostaddr=192.168.10.214 port=5432 dbname=mydb user=gearman password=gearman123' --libpq-table=gearmanqueue1 --verbose DEBUG -q Postgres -R'&",sep="")
