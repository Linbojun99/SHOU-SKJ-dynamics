

m4<-lmer(log_lat~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale,REML=T)
#m4<-glmmTMB(una_lat~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale)

summary(m4)

fixef(m4)


###确定参数的置信区间，boot法
c4<-confint(m4,method="boot",nsim=10000)
c4



plot_model(m4,type="re")+
  theme_bw()+
  scale_x_discrete()


plot(simulateResiduals(m4))




#plot_model(m4,type="eff",terms = "SSTG_Lat")+
# theme_bw()


###随机截距值
ranef(m4)


#不同随机效应水平下，xy关系
coef(m4)




library(MuMIn)
r.squaredGLMM(m4)



tab_model(m4)










data_scale1<-na.omit(data_scale)

#随机效应的显著性检验
m0<-lm(una_lat~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat,data=data_scale1)
m400<-lme(una_lat~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat,random=~1|fONI,data=data_scale1,method="ML")

summary(m0)

summary(m400)
library(RLRsim)
###随机效应的显著性检验
exactLRT(m=m400,m0=m0)

AIC(m0,m400)




plot(m4)
hist(residuals(m4))

performance::check_model(m4)


###方差提取
library(partR2)

R2m4 <- partR2(m4,  partvars = c("Rightegde_Lon", "Rightegde_Lat", "SSTG_Lon","SSTG_Lat"),
               R2_type = "marginal", nboot = 100, CI = 0.95,
               data = data_scale1)

forestplot(R2m4, type = "R2", line_size = 0.7, text_size = 14, point_size = 3)




###模型选择

options(na.action = na.fail)
m4_selection <- dredge(m4, evaluate = TRUE, rank = "AICc",REML=F)
m4_selection
subset(m4_selection, delta<2)

best_est4<-model.avg(m4_selection,subset= delta <2, revised.var = TRUE)
summary(best_est4)
sw(best_est4) 



m44<-lmer(log_lat~SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale,REML=T)
summary(m44)
tab_model(m44)

performance::check_model(m44)

ggsave(filename = "E:\\WarmpoolSKJ\\Figure\\model_check\\m44.svg",dpi=600,units = "in",width=16,height = 12)






















