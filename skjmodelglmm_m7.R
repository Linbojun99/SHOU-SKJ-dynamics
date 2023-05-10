

m7<-lmer(afad_lon~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale1,REML=T)
#m7<-glmmTMB(una_lat~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale)

summary(m7)

fixef(m7)


###确定参数的置信区间，boot法
c7<-confint(m7,method="boot",nsim=10000)
c7



plot_model(m7,type="re")+
  theme_bw()+
  scale_x_discrete()


plot(simulateResiduals(m7))




#plot_model(m7,type="eff",terms = "SSTG_Lat")+
# theme_bw()


###随机截距值
ranef(m7)


#不同随机效应水平下，xy关系
coef(m7)




library(MuMIn)
r.squaredGLMM(m7)



tab_model(m7)










data_scale1<-na.omit(data_scale)

#随机效应的显著性检验
m0<-lm(afad_lon~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat,data=data_scale1)
m700<-lme(afad_lon~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat,random=~1|fONI,data=data_scale1,method="ML")

summary(m0)

summary(m700)
library(RLRsim)
###随机效应的显著性检验
exactLRT(m=m700,m0=m0)

AIC(m0,m700)




plot(m7)
hist(residuals(m7))

performance::check_model(m7)


###方差提取
library(partR2)

R2m7 <- partR2(m7,  partvars = c("Rightegde_Lon", "Rightegde_Lat", "SSTG_Lon","SSTG_Lat"),
               R2_type = "marginal", nboot = 100, CI = 0.95,
               data = data_scale1)

forestplot(R2m7, type = "R2", line_size = 0.7, text_size = 14, point_size = 3)




###模型选择

options(na.action = na.fail)
m7_selection <- dredge(m7, evaluate = TRUE, rank = "AICc",REML=F)
m7_selection
subset(m7_selection, delta<2)

best_est7<-model.avg(m7_selection,subset= delta <2, revised.var = TRUE)
summary(best_est7)
sw(best_est7) 



m77<-lmer(afad_lon~SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale1,REML=T)
summary(m77)
tab_model(m77)
performance::check_model(m77)

ggsave(filename = "E:\\WarmpoolSKJ\\Figure\\model_check\\m77.svg",dpi=600,units = "in",width=16,height = 12)





















