

m8<-lmer(afad_lat~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale1,REML=T)
#m8<-glmmTMB(una_lat~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale)

summary(m8)

fixef(m8)


###确定参数的置信区间，boot法
c8<-confint(m8,method="boot",nsim=10000)
c8



plot_model(m8,type="re")+
  theme_bw()+
  scale_x_discrete()


plot(simulateResiduals(m8))




#plot_model(m8,type="eff",terms = "SSTG_Lat")+
# theme_bw()


###随机截距值
ranef(m8)


#不同随机效应水平下，xy关系
coef(m8)




library(MuMIn)
r.squaredGLMM(m8)



tab_model(m8)










data_scale1<-na.omit(data_scale)

#随机效应的显著性检验
m08<-lm(afad_lat~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat,data=data_scale1)
m800<-lme(afad_lat~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat,random=~1|fONI,data=data_scale1,method="ML")

summary(m08)

summary(m800)
library(RLRsim)
###随机效应的显著性检验
exactLRT(m=m800,m0=m08)

AIC(m08,m800)




plot(m8)
hist(residuals(m8))

performance::check_model(m8)


###方差提取
library(partR2)

R2m8 <- partR2(m8,  partvars = c("Rightegde_Lon", "Rightegde_Lat", "SSTG_Lon","SSTG_Lat"),
               R2_type = "marginal", nboot = 100, CI = 0.95,
               data = data_scale1)

forestplot(R2m8, type = "R2", line_size = 0.8, text_size = 14, point_size = 3)




###模型选择

options(na.action = na.fail)
m8_selection <- dredge(m08, evaluate = TRUE, rank = "AICc",REML=F)
m8_selection
subset(m8_selection, delta<2)

best_est8<-model.avg(m8_selection,subset= delta <2, revised.var = TRUE)
summary(best_est8)
sw(best_est8) 



m88<-lm(afad_lat~SSTG_Lon+SSTG_Lat,data=data_scale1)
summary(m88)
tab_model(m88)

performance::check_model(m88)
ggsave(filename = "E:\\WarmpoolSKJ\\Figure\\model_check\\m88.svg",dpi=600,units = "in",width=16,height = 12)






















