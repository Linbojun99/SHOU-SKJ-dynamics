#glmm
library(ggplot2)
library(nlme)
library(lme4)
library(glmmTMB)
library(sjPlot)
library(DHARMa)
library(MuMIn)
library(RLRsim)

data<-data %>%
  mutate(fONI= case_when(
    #Var=="una_lon"~1,
    ONI < -1.5~1,
    #Var=="log_lon"~2,
    ONI >= -1.5 & ONI< -0.5 ~2,
    #Var=="dfad_lon"~3,
    ONI >= -0.5 & ONI< 0.5 ~3,
    #Var=="afad_lon"~4,
    ONI >= 0.5 & ONI< 1.5 ~4,
    ONI >= 1.5~5
    
  ))

data_scale<-data[,"="(una_lon)]
data$FONI<-factor(data$ONI)



normalize <- function(x) {
  return ((x - min(x,na.rm=T)) / (max(x,na.rm=T) - min(x,na.rm=T)))
}
data_scale <- as.data.frame(lapply(data[,c(3:4,7:8,11,12,15,16,19,20,21,22,23,24)], normalize))
data_scale$Year=data$Year
data_scale$Month=data$Month
data_scale$ONI=data$ONI
data_scale$fONI=data$fONI

setwd("E:\\WarmpoolSKJ\\data")
data_scale<-read.csv("data_scale.csv")

m1<-lmer(una_lon~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale,REML=T)
#m1<-glmmTMB(una_lon~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale)

summary(m1)

fixef(m1)


###确定参数的置信区间，boot法
c1<-confint(m1,method="boot",nsim=10000)
c1



plot_model(m1,type="re",line.size=1.5,dot.size=3.5)+
  theme_bw()+
  scale_x_discrete()


plot(simulateResiduals(m11))




#plot_model(m1,type="eff",terms = "SSTG_Lat")+
# theme_bw()


###随机截距值
ranef(m1)


#不同随机效应水平下，xy关系
coef(m1)




r.squaredGLMM(m1)



tab_model(m1)










data_scale1<-na.omit(data_scale)

#随机效应的显著性检验
m0<-lm(una_lon~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat,data=data_scale1)
m100<-lme(una_lon~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat,random=~1|fONI,data=data_scale1,method="ML")

summary(m0)

summary(m100)
###随机效应的显著性检验
exactLRT(m=m100,m0=m0)

AIC(m0,m100)




plot(m1)
hist(residuals(m1))

performance::check_model(m1)


###方差提取
library(partR2)

R2m1 <- partR2(m1,  partvars = c("Rightegde_Lon", "Rightegde_Lat", "SSTG_Lon","SSTG_Lat"),
             R2_type = "marginal", nboot = 100, CI = 0.95,
             data = data_scale)

forestplot(R2m1, type = "R2", line_size = 0.7, text_size = 14, point_size = 3)




###模型选择

options(na.action = na.fail)
m1_selection <- dredge(m1, evaluate = TRUE, rank = "AICc",REML=F)
m1_selection
subset(m1_selection, delta<2)

best_est1<-model.avg(m1_selection,subset= delta <2, revised.var = TRUE)
summary(best_est1)
sw(best_est1) 



m11<-lmer(una_lon~Rightegde_Lon+Rightegde_Lat+SSTG_Lat+(1|fONI),data=data_scale,REML=T)
summary(m11)
tab_model(m11)
performance::check_model(m11)

ggsave(filename = "E:\\WarmpoolSKJ\\Figure\\model_check\\m11.svg",dpi=600,units = "in",width=16,height = 12)


###确定参数的置信区间，boot法
c11<-confint(m11,method="boot",nsim=10000)
c11






###方差提取
library(partR2)

R2m1 <- partR2(m11,  partvars = c("Rightegde_Lon", "Rightegde_Lat","SSTG_Lat"),
               R2_type = "marginal", nboot = 100, CI = 0.95,
               data = data_scale)

forestplot(R2m1, type = "R2", line_size = 0.7, text_size = 14, point_size = 3)











