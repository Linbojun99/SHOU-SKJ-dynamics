

m9<-lmer(Effort_lon~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale,REML=T)
#m9<-glmmTMB(una_lat~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale)

summary(m9)

fixef(m9)


###确定参数的置信区间，boot法
c9<-confint(m9,method="boot",nsim=10000)
c9



plot_model(m9,type="re")+
  theme_bw()+
  scale_x_discrete()


plot(simulateResiduals(m9))




#plot_model(m9,type="eff",terms = "SSTG_Lat")+
# theme_bw()


###随机截距值
ranef(m9)


#不同随机效应水平下，xy关系
coef(m9)




library(MuMIn)
r.squaredGLMM(m9)



tab_model(m9)










data_scale1<-na.omit(data_scale)

#随机效应的显著性检验
m09<-lm(Effort_lon~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat,data=data_scale1)
m900<-lme(Effort_lon~Rightegde_Lon+Rightegde_Lat+SSTG_Lon+SSTG_Lat,random=~1|fONI,data=data_scale1,method="ML")

summary(m09)

summary(m900)
library(RLRsim)
###随机效应的显著性检验
exactLRT(m=m900,m0=m09)

AIC(m09,m900)




plot(m9)
hist(residuals(m9))

performance::check_model(m9)


###方差提取
library(partR2)

R2m9 <- partR2(m9,  partvars = c("Rightegde_Lon", "Rightegde_Lat", "SSTG_Lon","SSTG_Lat"),
               R2_type = "marginal", nboot = 100, CI = 0.95,
               data = data_scale1)

forestplot(R2m9, type = "R2", line_size = 0.9, text_size = 14, point_size = 3)




###模型选择

options(na.action = na.fail)
m9_selection <- dredge(m9, evaluate = TRUE, rank = "AICc",REML=F)
m9_selection
subset(m9_selection, delta<2)

best_est9<-model.avg(m9_selection,subset= delta <2, revised.var = TRUE)
summary(best_est9)
sw(best_est9) 



m99<-lmer(Effort_lon ~Rightegde_Lat +SSTG_Lon+SSTG_Lat+(1|fONI),data=data_scale,REML=T)
summary(m99)
tab_model(m99)
performance::check_model(m99)
ggsave(filename = "E:\\WarmpoolSKJ\\Figure\\model_check\\m99.svg",dpi=600,units = "in",width=16,height = 12)






















