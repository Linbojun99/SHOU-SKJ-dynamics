#glmm
library(ggplot2)
library(nlme)
library(lme4)
library(glmmTMB)
library(sjPlot)



setwd("E:\\WarmpoolSKJ\\data")
data_scale<-read.csv("data_scale.csv")

m2<-lmer(una_lat~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale,REML=T)
#m2<-glmmTMB(una_lat~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale)

summary(m2)

fixef(m2)


###确定参数的置信区间，boot法
c2<-confint(m2,method="boot",nsim=10000)
c2



plot_model(m2,type="re")+
  theme_bw()+
  scale_x_discrete()


plot(simulateResiduals(m2))




#plot_model(m2,type="eff",terms = "SSTG_Lat")+
# theme_bw()


###随机截距值
ranef(m2)


#不同随机效应水平下，xy关系
coef(m2)




library(MuMIn)
r.squaredGLMM(m2)



tab_model(m2)










data_scale1<-na.omit(data_scale)

#随机效应的显著性检验
m0<-lm(una_lat~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat,data=data_scale1)
m200<-lme(una_lat~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat,random=~1|fONI,data=data_scale1,method="ML")

summary(m0)

summary(m200)
library(RLRsim)
###随机效应的显著性检验
exactLRT(m=m200,m0=m0)

AIC(m0,m200)




plot(m2)
hist(residuals(m2))

performance::check_model(m2)


###方差提取
library(partR2)

R2m2 <- partR2(m2,  partvars = c("Rightegde_Lon", "Rightegde_Lat", "SSTG_Lon","SSTG_Lat"),
               R2_type = "marginal", nboot = 100, CI = 0.95,
               data = data_scale)

forestplot(R2m2, type = "R2", line_size = 0.7, text_size = 14, point_size = 3)




###模型选择

options(na.action = na.fail)
m2_selection <- dredge(m2, evaluate = TRUE, rank = "AICc",REML=F)
m2_selection
subset(m2_selection, delta<2)

best_est2<-model.avg(m2_selection,subset= delta <2, revised.var = TRUE)
summary(best_est2)
sw(best_est2) 



m22<-lmer(una_lat~SSTG_Lat+(1|fONI),data=data_scale,REML=T)
summary(m22)
tab_model(m22)

performance::check_model(m22)

ggsave(filename = "E:\\WarmpoolSKJ\\Figure\\model_check\\m22.svg",dpi=600,units = "in",width=16,height = 12)



###确定参数的置信区间，boot法
c22<-confint(m22,method="boot",nsim=10000)
c22

















