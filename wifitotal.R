##��Ŀ���ƣ�EMC����
##��Ŀʱ�䣺2015-4-20
##��Ŀ�����ˣ�
##��ĿĿ�ģ��о��Ϻ�����WiFi���
##��Ŀ���������Ʒ���\��ͼ


##���ݴ����mysql��
##���ذ�
library(DBI)
library(RMySQL)
library(ggplot2)
##��ջ���
rm(list=ls())
##�������ݿ�
con <- dbConnect(RMySQL::MySQL(), dbname = "emc",user="root")
##ȡ������������
rs <- dbGetQuery(con, "SELECT FROM_UNIXTIME(start_time/1000,'%Y-%m-%d') AS start_date,
sum(bytes)/1024/1024 AS MB FROM traffic GROUP BY start_date")
##����x������
datebreaks <- seq(as.Date("2014-09-01"),as.Date("2015-01-31"),by="2 week")
##��wifiÿ�����������ͼ
p <- ggplot(rs,aes(x=as.Date(start_date),y=MB))+geom_line()
##�޸�ͼ��
p+theme_bw()+labs(x="",y="bytes(M)")+scale_x_date(breaks=datebreaks)+theme(axis.text.x=element_text(angle=30,hjust=1))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

##ֱ�Ӷ�ȡ��ֺ���ļ�
##���ذ�
library(ggplot)
##��ջ���
rm(list=ls())
##��ȡ�ļ�
traffic <- read.csv(file="spilttraffic.txt",fileEncoding="UTF-8",
                header=FALSE,sep=";",stringsAsFactors=FALSE)
##��������
names(traffic)=c("id","location","start_time","duration","service_provider","service_type","domation","bytes","https")
##ÿ����������з������
rs <- traffic[,c("start_time","bytes")]
rs$start_date <- as.Date(strptime(as.POSIXlt(rs$start_time/1000,origin="1970-01-01"),format="%Y-%m-%d"))
rs$MB <- rs$bytes/1024/1024
data <- aggregate(MB ~ start_date, rs, sum)
##����x����
datebreaks <- seq(as.Date("2014-09-01"),as.Date("2015-01-31"),by="2 week")
##��wifiÿ�����������ͼ
p <- ggplot(data,aes(x=start_date,y=MB))+geom_line()
##�޸�ͼ�����
p+theme_bw()+labs(x="",y="bytes(M)")+scale_x_date(breaks=datebreaks)+theme(axis.text.x=element_text(angle=30,hjust=1))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

