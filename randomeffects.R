#random effect
r1<-plot_model(m11,type="re",show.values = T)+
  theme_bw()+
  scale_x_discrete()


r2<-plot_model(m22,type="re",show.values = T)+
  theme_bw()+
  scale_x_discrete()


r3<-plot_model(m33,type="re",show.values = T)+
  theme_bw()+
  scale_x_discrete()

r4<-plot_model(m44,type="re",show.values = T)+
  theme_bw()+
  scale_x_discrete()



r5<-plot_model(m55,type="re",show.values = T)+
  theme_bw()+
  scale_x_discrete()


r6<-plot_model(m66,type="re",show.values = T)+
  theme_bw()+
  scale_x_discrete()



r7<-plot_model(m77,type="re",show.values = T)+
  theme_bw()+
  scale_x_discrete()


r8<-plot_model(m881,type="re",show.values = T)+
  theme_bw()+
  scale_x_discrete()


r9<-plot_model(m99,type="re",show.values = T)+
  theme_bw()+
  scale_x_discrete()


r10<-plot_model(m1010,type="re",show.values = T)+
  theme_bw()+
  scale_x_discrete()


library(cowplot)
library(eoffice)

plot_grid(r1,r2,r3,r4,r5,r6,r7,r9,r10,labels = "AUTO",nrow=3)

topptx(filename = "E:\\WarmpoolSKJ\\Figure\\re.pptx",units = "in",width=9,height = 9)




