

m6<-lmer(dfad_lat~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale1,REML=T)
#m6<-glmmTMB(una_lat~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale)

summary(m6)

fixef(m6)


###确定参数的置信区间，boot法
c6<-confint(m6,method="boot",nsim=10000)
c6



plot_model(m6,type="re")+
  theme_bw()+
  scale_x_discrete()


plot(simulateResiduals(m6))




#plot_model(m6,type="eff",terms = "SSTG_Lat")+
# theme_bw()


###随机截距值
ranef(m6)


#不同随机效应水平下，xy关系
coef(m6)




library(MuMIn)
r.squaredGLMM(m6)



tab_model(m6)










data_scale1<-na.omit(data_scale)

#随机效应的显著性检验
m0<-lm(dfad_lat~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat,data=data_scale1)
m600<-lme(dfad_lat~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat,random=~1|fONI,data=data_scale1,method="ML")

summary(m0)

summary(m600)
library(RLRsim)
###随机效应的显著性检验
exactLRT(m=m600,m0=m0)

AIC(m0,m600)




plot(m6)
hist(residuals(m6))

performance::check_model(m6)


###方差提取
library(partR2)

R2m6 <- partR2(m6,  partvars = c("Rightegde_Lon", "Rightegde_Lat", "SSTG_Lon","SSTG_Lat"),
               R2_type = "marginal", nboot = 100, CI = 0.95,
               data = data_scale1)

forestplot(R2m6, type = "R2", line_size = 0.7, text_size = 14, point_size = 3)




###模型选择

options(na.action = na.fail)
m6_selection <- dredge(m6, evaluate = TRUE, rank = "AICc",REML=F)
m6_selection
subset(m6_selection, delta<2)

best_est6<-model.avg(m6_selection,subset= delta <2, revised.var = TRUE)
summary(best_est6)
sw(best_est6) 



m66<-lmer(dfad_lat~SSTG_Lat+(1|fONI),data=data_scale1,REML=T)
summary(m66)
tab_model(m66)
performance::check_model(m66)
ggsave(filename = "E:\\WarmpoolSKJ\\Figure\\model_check\\m66.svg",dpi=600,units = "in",width=16,height = 12)






















