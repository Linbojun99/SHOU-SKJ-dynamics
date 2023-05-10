

m10<-lmer(Effort_lat~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale,REML=T)
#m10<-glmmTMB(una_lat~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale)

summary(m10)

fixef(m10)


###确定参数的置信区间，boot法
c10<-confint(m10,method="boot",nsim=10000)
c10



plot_model(m10,type="re")+
  theme_bw()+
  scale_x_discrete()


plot(simulateResiduals(m10))




#plot_model(m10,type="eff",terms = "SSTG_Lat")+
# theme_bw()


###随机截距值
ranef(m10)


#不同随机效应水平下，xy关系
coef(m10)




library(MuMIn)
r.squaredGLMM(m10)



tab_model(m10)










data_scale1<-na.omit(data_scale)

#随机效应的显著性检验
m010<-lm(Effort_lat~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat,data=data_scale1)
m1000<-lme(Effort_lat~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat,random=~1|fONI,data=data_scale1,method="ML")

summary(m010)

summary(m1000)
library(RLRsim)
###随机效应的显著性检验
exactLRT(m=m1000,m0=m010)

AIC(m010,m1000)




plot(m10)
hist(residuals(m10))

performance::check_model(m10)


###方差提取
library(partR2)

R2m10 <- partR2(m10,  partvars = c("Rightegde_Lon", "Rightegde_Lat", "SSTG_Lon","SSTG_Lat"),
               R2_type = "marginal", nboot = 100, CI = 0.105,
               data = data_scale1)

forestplot(R2m10, type = "R2", line_size = 0.10, text_size = 14, point_size = 3)




###模型选择

options(na.action = na.fail)
m10_selection <- dredge(m10, evaluate = TRUE, rank = "AICc",REML=F)
m10_selection
subset(m10_selection, delta<2)

best_est10<-model.avg(m10_selection,subset= delta <2, revised.var = TRUE)
summary(best_est10)
sw(best_est10) 



m1010<-lmer(Effort_lat ~SSTG_Lat+(1|fONI),data=data_scale,REML=T)
summary(m1010)
tab_model(m1010)
performance::check_model(m1010)


ggsave(filename = "E:\\WarmpoolSKJ\\Figure\\model_check\\m1010.svg",dpi=600,units = "in",width=16,height = 12)




















