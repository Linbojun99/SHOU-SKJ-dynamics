#glmm
library(ggplot2)
library(nlme)
library(lme4)
library(glmmTMB)
library(sjPlot)



setwd("E:\\WarmpoolSKJ\\data")
data_scale<-read.csv("data_scale.csv")

###lon
###lat
m3<-lmer(log_lon~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale,REML=T)
#m3<-glmmTMB(una_lat~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale)

summary(m3)

fixef(m3)


###确定参数的置信区间，boot法
c3<-confint(m3,method="boot",nsim=10000)
c3



plot_model(m3,type="re")+
  theme_bw()+
  scale_x_discrete()


plot(simulateResiduals(m3))




#plot_model(m3,type="eff",terms = "SSTG_Lat")+
# theme_bw()


###随机截距值
ranef(m3)


#不同随机效应水平下，xy关系
coef(m3)




library(MuMIn)
r.squaredGLMM(m3)



tab_model(m3)










data_scale1<-na.omit(data_scale)

#随机效应的显著性检验
m0<-lm(log_lon~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat,data=data_scale1)
m300<-lme(log_lon~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat,random=~1|fONI,data=data_scale1,method="ML")

summary(m0)

summary(m300)
library(RLRsim)
###随机效应的显著性检验
exactLRT(m=m300,m0=m0)

AIC(m0,m300)




plot(m3)
hist(residuals(m3))

performance::check_model(m3)


###方差提取
library(partR2)

R2m3 <- partR2(m3,  partvars = c("Rightegde_Lon", "Rightegde_Lat", "SSTG_Lon","SSTG_Lat"),
               R2_type = "marginal", nboot = 100, CI = 0.95,
               data = data_scale1)

forestplot(R2m3, type = "R2", line_size = 0.7, text_size = 14, point_size = 3)




###模型选择

options(na.action = na.fail)
m3_selection <- dredge(m3, evaluate = TRUE, rank = "AICc",REML=F)
m3_selection
subset(m3_selection, delta<2)

best_est3<-model.avg(m3_selection,subset= delta <2, revised.var = TRUE)
summary(best_est3)
sw(best_est3) 



m33<-lmer(log_lon~Rightegde_Lon+Rightegde_Lat+(1|fONI),data=data_scale,REML=T)
summary(m33)
tab_model(m33)

performance::check_model(m33)

ggsave(filename = "E:\\WarmpoolSKJ\\Figure\\model_check\\m33.svg",dpi=600,units = "in",width=16,height = 12)

###确定参数的置信区间，boot法
c33<-confint(m33,method="boot",nsim=10000)
c33





















