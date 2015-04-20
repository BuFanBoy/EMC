##项目名称：EMC比赛
##项目时间：2015-4-20
##项目负责人：
##项目目的：研究上海交大WiFi情况
##项目方法：趋势分析\绘图


##数据存放在mysql中
##加载包
library(DBI)
library(RMySQL)
library(ggplot2)
##清空缓存
rm(list=ls())
##连接数据库
con <- dbConnect(RMySQL::MySQL(), dbname = "emc",user="root")
##取分析所需数据
rs <- dbGetQuery(con, "SELECT FROM_UNIXTIME(start_time/1000,'%Y-%m-%d') AS start_date,
sum(bytes)/1024/1024 AS MB FROM traffic GROUP BY start_date")
##定义x轴间隔点
datebreaks <- seq(as.Date("2014-09-01"),as.Date("2015-01-31"),by="2 week")
##作wifi每天的流量走势图
p <- ggplot(rs,aes(x=as.Date(start_date),y=MB))+geom_line()
##修改图形
p+theme_bw()+labs(x="",y="bytes(M)")+scale_x_date(breaks=datebreaks)+theme(axis.text.x=element_text(angle=30,hjust=1))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

##直接读取拆分后的文件
##加载包
library(ggplot)
##清空缓存
rm(list=ls())
##读取文件
traffic <- read.csv(file="spilttraffic.txt",fileEncoding="UTF-8",
                header=FALSE,sep=";",stringsAsFactors=FALSE)
##给列命名
names(traffic)=c("id","location","start_time","duration","service_provider","service_type","domation","bytes","https")
##每天的流量进行分组求合
rs <- traffic[,c("start_time","bytes")]
rs$start_date <- as.Date(strptime(as.POSIXlt(rs$start_time/1000,origin="1970-01-01"),format="%Y-%m-%d"))
rs$MB <- rs$bytes/1024/1024
data <- aggregate(MB ~ start_date, rs, sum)
##定义x轴间隔
datebreaks <- seq(as.Date("2014-09-01"),as.Date("2015-01-31"),by="2 week")
##作wifi每天的流量走势图
p <- ggplot(data,aes(x=start_date,y=MB))+geom_line()
##修改图形外观
p+theme_bw()+labs(x="",y="bytes(M)")+scale_x_date(breaks=datebreaks)+theme(axis.text.x=element_text(angle=30,hjust=1))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())


