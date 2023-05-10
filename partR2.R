#plot


###方差提取
library(partR2)
R2m1 <- partR2(m11,  partvars = c("Rightegde_Lon", "Rightegde_Lat","SSTG_Lat"),
               R2_type = "marginal", nboot = 100, CI = 0.95,
               data = data_scale)

f1<-forestplot(R2m1, type = "R2", line_size = 1.5, text_size = 8, point_size = 3)

R2m2 <- partR2(m22,  partvars = c("SSTG_Lat"),
               R2_type = "marginal", nboot = 100, CI = 0.95,
               data = data_scale)
f2<-forestplot(R2m2, type = "R2", line_size = 1.5, text_size = 8, point_size = 3)


R2m3 <- partR2(m33,  partvars = c("Rightegde_Lon", "Rightegde_Lat"),
               R2_type = "marginal", nboot = 100, CI = 0.95,
               data = data_scale)

f3<-forestplot(R2m3, type = "R2", line_size = 1.5, text_size = 8, point_size = 3)




R2m4 <- partR2(m44,  partvars = c("SSTG_Lon", "SSTG_Lat"),
               R2_type = "marginal", nboot = 100, CI = 0.95,
               data = data_scale)

f4<-forestplot(R2m4, type = "R2", line_size = 1.5, text_size = 8, point_size = 3)


R2m5 <- partR2(m55,  partvars = c("Rightegde_Lat","SSTG_Lon", "SSTG_Lat"),
               R2_type = "marginal", nboot = 100, CI = 0.95,
               data = data_scale1)

f5<-forestplot(R2m5, type = "R2", line_size = 1.5, text_size = 8, point_size = 3)

R2m6 <- partR2(m66,  partvars = c( "SSTG_Lat"),
               R2_type = "marginal", nboot = 100, CI = 0.95,
               data = data_scale1)

f6<-forestplot(R2m6, type = "R2", line_size = 1.5, text_size = 8, point_size = 3)



R2m7 <- partR2(m77,  partvars = c("SSTG_Lon", "SSTG_Lat"),
               R2_type = "marginal", nboot = 100, CI = 0.95,
               data = data_scale1)

f7<-forestplot(R2m7, type = "R2", line_size = 1.5, text_size = 8, point_size = 3)



m881<-lmer(afad_lat~SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale1,REML=T)


R2m8 <- partR2(m881,  partvars = c("SSTG_Lon", "SSTG_Lat"),
               R2_type = "marginal", nboot = 100, CI = 0.95,
               data = data_scale1)

f8<-forestplot(R2m8, type = "R2", line_size = 1.5, text_size = 8, point_size = 3)



R2m9 <- partR2(m99,  partvars = c("Rightegde_Lat","SSTG_Lon", "SSTG_Lat"),
               R2_type = "marginal", nboot = 100, CI = 0.95,
               data = data_scale)

f9<-forestplot(R2m9, type = "R2", line_size = 1.5, text_size = 8, point_size = 3)

R2m10 <- partR2(m1010,  partvars = c("SSTG_Lat"),
               R2_type = "marginal", nboot = 100, CI = 0.95,
               data = data_scale)

f10<-forestplot(R2m10, type = "R2", line_size = 1.5, text_size = 8, point_size = 3)




library(cowplot)
library(eoffice)
plot_grid(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,labels="AUTO",nrow = 5)
ggsave(filename = "E:\\WarmpoolSKJ\\Figure\\model_check\\f.svg",dpi=600,units = "in",width=9,height = 16)
topptx(filename = "E:\\WarmpoolSKJ\\Figure\\model_check\\f1.pptx",units = "in",width=8,height = 20)



plot_grid(f1,f3,f4,f5,f7,f8,f9,labels="AUTO",ncol = 7)
topptx(filename = "E:\\WarmpoolSKJ\\Figure\\model_check\\f1.pptx",units = "in",width=60,height = 6)



plot_grid(f2,f6,f10,labels="AUTO",nrow = 3)
topptx(filename = "E:\\WarmpoolSKJ\\Figure\\model_check\\f1.pptx",units = "in",width=4,height = 32)
