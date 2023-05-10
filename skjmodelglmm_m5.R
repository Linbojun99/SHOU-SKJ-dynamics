

m5<-lmer(dfad_lon~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale1,REML=T)
#m5<-glmmTMB(una_lat~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale)

summary(m5)

fixef(m5)


###确定参数的置信区间，boot法
c5<-confint(m5,method="boot",nsim=10000)
c5



plot_model(m5,type="re")+
  theme_bw()+
  scale_x_discrete()


plot(simulateResiduals(m5))




#plot_model(m5,type="eff",terms = "SSTG_Lat")+
# theme_bw()


###随机截距值
ranef(m5)


#不同随机效应水平下，xy关系
coef(m5)




library(MuMIn)
r.squaredGLMM(m5)



tab_model(m5)










data_scale1<-na.omit(data_scale)

#随机效应的显著性检验
m05<-lm(dfad_lon~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat,data=data_scale1)
m500<-lme(dfad_lon~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat,random=~1|fONI,data=data_scale1,method="ML")

summary(m05)

summary(m500)
library(RLRsim)
###随机效应的显著性检验
exactLRT(m=m500,m0=m05)

AIC(m05,m500)




plot(m5)
hist(residuals(m5))

performance::check_model(m5)


###方差提取
library(partR2)

R2m5 <- partR2(m5,  partvars = c("Rightegde_Lon", "Rightegde_Lat", "SSTG_Lon","SSTG_Lat"),
               R2_type = "marginal", nboot = 100, CI = 0.95,
               data = data_scale1)

forestplot(R2m5, type = "R2", line_size = 0.7, text_size = 14, point_size = 3)




###模型选择

options(na.action = na.fail)
m5_selection <- dredge(m5, evaluate = TRUE, rank = "AICc",REML=F)
m5_selection
subset(m5_selection, delta<2)

best_est5<-model.avg(m5_selection,subset= delta <2, revised.var = TRUE)
summary(best_est5)
sw(best_est5) 



m55<-lmer(dfad_lon~SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale1,REML=T)
m55<-lmer(dfad_lon~Rightegde_Lat+SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale1,REML=T)
summary(m55)
tab_model(m55)

performance::check_model(m55)
ggsave(filename = "E:\\WarmpoolSKJ\\Figure\\model_check\\m55.svg",dpi=600,units = "in",width=16,height = 12)






















