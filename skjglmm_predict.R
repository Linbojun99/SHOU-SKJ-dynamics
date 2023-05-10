library(dplyr)
library(tidyverse)
library(ggsci)
library(metR)
#Predict
Rightedge2020_2100<-read.csv("E:\\WarmpoolSKJ\\data\\Rightedge2020_2100.csv",T)

SSTG2020_2100<-read.csv("E:\\WarmpoolSKJ\\data\\SSTG2020_2100.csv",T)

# 归一化数据
normalize_data <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}




WP2020_2100<- merge(Rightedge2020_2100, SSTG2020_2100, by = c("Year", "Month"))

WP2020_2100_scale <- apply(WP2020_2100, 2, normalize_data)

WP2020_2100_scale<-as.data.frame(WP2020_2100_scale)


# 自定义自助估计函数
boot_func <- function(model, new_data) {
  return(function(...) {
    predict(model, newdata = new_data, re.form = ~0)
  })
}

# 设置随机种子以获得可重现的结果
set.seed(1234)

# 使用bootMer()进行自助估计
boot_results1 <- bootMer(
  m11,
  FUN = boot_func(m11, WP2020_2100_scale),
  nsim = 10000
)

# 计算置信区间
confidence_intervals1 <- t(apply(boot_results1$t, 2, function(x) {
  quantile(x, c(0.05, 0.975))
}))


una_lon_ci_low<-inverse_normalize_data(confidence_intervals1[,1], data$una_lon)
una_lon_ci_high<-inverse_normalize_data(confidence_intervals1[,2], data$una_lon)





una_lon<-predict(m11,WP2020_2100_scale,re.form = ~0)
una_lat<-predict(m22,WP2020_2100_scale,re.form = ~0)


log_lon<-predict(m33,WP2020_2100_scale,re.form = ~0)
log_lat<-predict(m44,WP2020_2100_scale,re.form = ~0)


dfad_lon<-predict(m55,WP2020_2100_scale,re.form = ~0)
dfad_lat<-predict(m66,WP2020_2100_scale,re.form = ~0)

afad_lon<-predict(m77,WP2020_2100_scale,re.form = ~0)
afad_lat<-predict(m88,WP2020_2100_scale,re.form = ~0)



effort_lon<-predict(m99,WP2020_2100_scale,re.form = ~0)
effort_lat<-predict(m1010,WP2020_2100_scale,re.form = ~0)

#data<-read.csv("E:\\WarmpoolSKJ\\data\\data.csv",T)

###逆归一化
# 逆归一化数据
inverse_normalize_data <- function(x_normalized, x_original) {
  return(x_normalized * (max(x_original) - min(x_original)) + min(x_original))
}


#Una
Una_lon <- inverse_normalize_data(una_lon, data$una_lon)
Una_lat <- inverse_normalize_data(una_lat, data$una_lat)

Una<-cbind(WP2020_2100[,1:2],Una_lon,Una_lat)
Una <- Una[order(Una$Month), ]
Una <- Una[order(Una$Year), ]


#Log
Log_lon <- inverse_normalize_data(log_lon, data$log_lon)
Log_lat <- inverse_normalize_data(log_lat, data$log_lat)

Log<-cbind(WP2020_2100[,1:2],Log_lon,Log_lat)
Log <- Log[order(Log$Month), ]
Log <- Log[order(Log$Year), ]

data1<-na.omit(data)
#dFAD
dFAD_lon <- inverse_normalize_data(dfad_lon, data1$dfad_lon)
dFAD_lat <- inverse_normalize_data(dfad_lat, data1$dfad_lat)

dFAD<-cbind(WP2020_2100[,1:2],dFAD_lon,dFAD_lat)
dFAD <- dFAD[order(dFAD$Month), ]
dFAD <- dFAD[order(dFAD$Year), ]


#aFAD
aFAD_lon <- inverse_normalize_data(afad_lon, data1$afad_lon)
aFAD_lat <- inverse_normalize_data(afad_lat, data1$afad_lat)

aFAD<-cbind(WP2020_2100[,1:2],aFAD_lon,aFAD_lat)
aFAD <- aFAD[order(aFAD$Month), ]
aFAD <- aFAD[order(aFAD$Year), ]


#Effort
Effort_lon <- inverse_normalize_data(effort_lon, data$Effort_lon)
Effort_lat <- inverse_normalize_data(effort_lat, data$Effort_lat)

Effort<-cbind(WP2020_2100[,1:2],Effort_lon,Effort_lat)
Effort <- Effort[order(Effort$Month), ]
Effort <- Effort[order(Effort$Year), ]





Una1<-Una[,1:4]
colnames(Una1)[3]<-"Lon"
colnames(Una1)[4]<-"Lat"
Una1$Type<-"Una"


Log1<-Log
colnames(Log1)[3]<-"Lon"
colnames(Log1)[4]<-"Lat"
Log1$Type<-"Log"


dFAD1<-dFAD
colnames(dFAD1)[3]<-"Lon"
colnames(dFAD1)[4]<-"Lat"
dFAD1$Type<-"dFAD"

aFAD1<-aFAD
colnames(aFAD1)[3]<-"Lon"
colnames(aFAD1)[4]<-"Lat"
aFAD1$Type<-"aFAD"


Effort1<-Effort
colnames(Effort1)[3]<-"Lon"
colnames(Effort1)[4]<-"Lat"
Effort1$Type<-"Effort"

alltype<-rbind(Una1,Log1,dFAD1,aFAD1,Effort1)
alltype_y<- alltype %>%
  group_by(Year,Type) %>%
  summarize(
    avg_longitude = mean(Lon),
    avg_latitude = mean(Lat),
    .groups = "drop"
  )


alltype_y$Type<-factor(alltype_y$Type,
       levels = c("Una","Log","dFAD","aFAD","Effort"))


#write.csv(alltype,"E:\\WarmpoolSKJ\\data\\predict2020_2100.csv",row.names = F)
###plot
a1<-ggplot(alltype_y, aes(y = Year, x = avg_longitude, color = Type)) +
  geom_path(linewidth=1.5) +
  geom_point(size=3,shape=1)+
  #facet_grid(Type~.) +
  labs(
    #title = "Annual Longitude Changes per Type",
    y = "Year",
    x = "Longitude"
  ) +
  theme_bw()+
  scale_color_d3()+
  scale_x_longitude(name="Longitude",breaks = seq(150,170,2.5))+
  scale_y_continuous(expand=c(0,0),breaks = seq(2020,2100,5))+
  theme(legend.position = "None")+
  theme(plot.margin=unit(rep(1,4),'lines'))




ggsave(filename = "E:\\WarmpoolSKJ\\Figure\\20202100lon.tiff",dpi=600)

a2<-ggplot(alltype_y, aes(x = Year, y = avg_latitude, color = Type)) +
  geom_path(linewidth=1.5) +
  geom_point(size=3,shape=1)+
  #facet_grid(Type~.) +
  labs(
    #title = "Annual Latitude Changes per Type",
    x = "Year",
    y = "Latitude"
  ) +
  theme_bw()+
  scale_color_d3()+
  scale_y_latitude(name="Latitude",breaks = seq(-5,0,0.25))+
  scale_x_continuous(expand=c(0,0),breaks = seq(2020,2100,5))+
  theme(legend.position = "bottom")+
  theme(plot.margin=unit(rep(1,4),'lines'))
  


  
ggsave(filename = "E:\\WarmpoolSKJ\\Figure\\20202100lat.tiff",dpi=600)




plot_grid(a1,a2,nrow=2,labels = "AUTO",rel_heights =c(2,1.5))
ggsave(filename = "E:\\WarmpoolSKJ\\Figure\\20202100lonlat.tiff",dpi=600,units = "in",width = 9,height = 15)
topptx(filename = "E:\\WarmpoolSKJ\\Figure\\20202100lonlat.pptx",units = "in",width=9,height = 15)



Effort_pre<-subset(alltype_y,alltype_y$Type=="Effort")
Una_pre<-subset(alltype_y,alltype_y$Type=="Una")
Log_pre<-subset(alltype_y,alltype_y$Type=="Log")
dFAD_pre<-subset(alltype_y,alltype_y$Type=="dFAD")
aFAD_pre<-subset(alltype_y,alltype_y$Type=="aFAD")



cor1_lon<- cor.test(Effort_pre$avg_longitude,Una_pre$avg_longitude)
cor1_lat<-cor.test(Effort_pre$avg_latitude,Una_pre$avg_latitude)


cor2_lon<- cor.test(Effort_pre$avg_longitude,Log_pre$avg_longitude)
cor2_lat<-cor.test(Effort_pre$avg_latitude,Log_pre$avg_latitude)


cor3_lon<- cor.test(Effort_pre$avg_longitude,dFAD_pre$avg_longitude)
cor3_lat<-cor.test(Effort_pre$avg_latitude,dFAD_pre$avg_latitude)


cor4_lon<- cor.test(Effort_pre$avg_longitude,aFAD_pre$avg_longitude)
cor4_lat<-cor.test(Effort_pre$avg_latitude,aFAD_pre$avg_latitude)


















point10<-subset(alltype_y,alltype_y$Year==2020|alltype_y$Year==2030|
                alltype_y$Year==2040|
                  alltype_y$Year==2050|
                  alltype_y$Year==2060|
                  alltype_y$Year==2070|
                  alltype_y$Year==2080|
                  alltype_y$Year==2090|
                  alltype_y$Year==2100)







library(ggplot2)
library(ggspatial)
library(sf)
library(maps)
library(metR)
library(ggthemes)
library(grDevices) 
library(RColorBrewer)
library(ggforce)

#常规地图world 
#太平洋地图world2
worldMap <- fortify(map_data("world2"), region = "subregion")


##调色
colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(106)


p<-ggplot() + 
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='black',color='black',size=0.5,alpha=1)+
  geom_point(data=alltype_y,aes(x=avg_longitude,y=avg_latitude,color=Type),shape=1,size=2)+#z要填充的变量 
  scale_color_d3()+
  #scale_color_material("red")+
  theme_bw()+
  #facet_grid(Type~.)+
 
  #你的区域
  coord_sf(ylim=c(-10,0),xlim=c(145,175))+
  scale_y_latitude(name='Latitude',breaks = seq(-10,10,2))+
  scale_x_longitude(name='Longitude',breaks = seq(145,175,5))










library(dplyr)

# 创建一个新变量，将年份分组为相应的十年区间
alltype_y$decade <- cut(alltype_y$Year, breaks = seq(2020, 2110, by = 10), right = FALSE, include.lowest = TRUE)

# 计算每个十年区间的平均值
my_data_decade_mean <- alltype_y %>%
  group_by(decade,Type) %>%
  summarize(mean_lon = mean(avg_longitude, na.rm = TRUE),
            mean_lat = mean(avg_latitude, na.rm = TRUE))

my_data_decade_mean$Year<-point10$Year




#aFAD
p1<-ggplot() + 
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='black',color='black',size=0.5,alpha=1)+
  geom_point(data=my_data_decade_mean[my_data_decade_mean$Type=="aFAD",],aes(x=mean_lon,y=mean_lat,color=Year),size=5)+#z要填充的变量 
  #scale_color_d3()+
  scale_color_material("red")+
  theme_bw()+
  #facet_grid(Type~.)+
  #你的区域
  coord_sf(ylim=c(-4.4,-4),xlim=c(151.4,152.7))+
  geom_text(data=my_data_decade_mean[my_data_decade_mean$Type=="aFAD",],aes(x=mean_lon,y=mean_lat,label=Year),nudge_y=0.02)+
  
  scale_y_latitude(name='Latitude',breaks = seq(-4.4,-4,0.1))+
  scale_x_longitude(name='Longitude',breaks = seq(151.4,152.7,0.2))+
  theme(legend.position = "top")




#Log
p2<-ggplot() + 
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='black',color='black',size=0.5,alpha=1)+
  geom_point(data=my_data_decade_mean[my_data_decade_mean$Type=="Log",],aes(x=mean_lon,y=mean_lat,color=Year),size=5)+#z要填充的变量 
  #scale_color_d3()+
  scale_color_material("orange")+
  theme_bw()+
  #facet_grid(Type~.)+
  #你的区域
  coord_sf(ylim=c(-1.8,-1.3),xlim=c(159.4,160.2))+
  geom_text(data=my_data_decade_mean[my_data_decade_mean$Type=="Log",],aes(x=mean_lon,y=mean_lat,label=Year),nudge_y=0.02)+
  
  scale_y_latitude(name='Latitude',breaks = seq(-1.8,-1.3,0.1))+
  scale_x_longitude(name='Longitude',breaks = seq(159,175,0.2))+
  theme(legend.position = "top")





#Una
p3<-ggplot() + 
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='black',color='black',size=0.5,alpha=1)+
  geom_point(data=my_data_decade_mean[my_data_decade_mean$Type=="Una",],aes(x=mean_lon,y=mean_lat,color=Year),size=5)+#z要填充的变量 
  #scale_color_d3()+
  scale_color_material("blue")+
  theme_bw()+
  #facet_grid(Type~.)+
  #你的区域
  coord_sf(ylim=c(-2.6,-2.2),xlim=c(165.2,166.2))+
  geom_text(data=my_data_decade_mean[my_data_decade_mean$Type=="Una",],aes(x=mean_lon,y=mean_lat,label=Year),nudge_y=0.02)+
  
  scale_y_latitude(name='Latitude',breaks = seq(-2.8,-2.2,0.1))+
  scale_x_longitude(name='Longitude',breaks = seq(159,175,0.2))+
  theme(legend.position = "bottom")







#dFAD
p4<-ggplot() + 
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='black',color='black',size=0.5,alpha=1)+
  geom_point(data=my_data_decade_mean[my_data_decade_mean$Type=="dFAD",],aes(x=mean_lon,y=mean_lat,color=Year),size=5)+#z要填充的变量 
  #scale_color_d3()+
  scale_color_material("green")+
  theme_bw()+
  #facet_grid(Type~.)+
  #你的区域
  coord_sf(ylim=c(-2.5,-2),xlim=c(168.7,170.3))+
  geom_text(data=my_data_decade_mean[my_data_decade_mean$Type=="dFAD",],aes(x=mean_lon,y=mean_lat,label=Year),nudge_y=0.02)+
  
  scale_y_latitude(name='Latitude',breaks = seq(-2.5,2,0.1))+
  scale_x_longitude(name='Longitude',breaks = seq(168.5,170.5,0.2))+
  theme(legend.position = "bottom")








#Effort
p5<-ggplot() + 
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill='black',color='black',size=0.5,alpha=1)+
  geom_point(data=my_data_decade_mean[my_data_decade_mean$Type=="Effort",],aes(x=mean_lon,y=mean_lat,color=Year),size=5)+#z要填充的变量 
  #scale_color_d3()+
  scale_color_material("purple")+
  theme_bw()+
  #facet_grid(Type~.)+
  #你的区域
  coord_sf(ylim=c(-2.5,-1.9),xlim=c(163.5,164.9))+
  geom_text(data=my_data_decade_mean[my_data_decade_mean$Type=="Effort",],aes(x=mean_lon,y=mean_lat,label=Year),nudge_y=0.02)+
  scale_y_latitude(name='Latitude',breaks = seq(-2.5,2,0.1))+
  scale_x_longitude(name='Longitude',breaks = seq(163,170.5,0.2))




# 加载所需的包
library(gridExtra)
# 自定义布局矩阵
layout_matrix <- rbind(
  c(1, 1, 2, 2),
  c(3, 3, 4, 4),
  c(5, 5, 6, 6)
)

layout_matrix <- rbind(
  c(0, 1, 1, 2, 2, 0),
  c(3, 3, 3, 3, 4, 4),
  c(0, 5, 5, 6, 6, 0)
)

grid.arrange(p1, p2, p, p5, p3, p4, layout_matrix = layout_matrix)


plot_grid(NULL,p1,p2,NULL,
           
          NULL,p,NULL, p3 ,
  NULL,p4, p5,NULL, 
  
  nrow = 3, ncol = 4,
  rel_widths = c(2,3,3, 2),
  rel_heights = c(3,3, 3)
)

ggsave(filename = "E:\\WarmpoolSKJ\\Figure\\predis.tiff",dpi=600,units = "in",width = 9,height = 18)
topptx(filename = "E:\\WarmpoolSKJ\\Figure\\predis.pptx",units = "in",width=12,height = 15)
