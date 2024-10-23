# author: KR
# date: 10/23/24
# purpose: make metals and speciated As datasets ready to work with

library(tidyverse)
library(openxlsx)
library(patchwork)

#read in baseline regression results
longMod1reg4s1_TW<-read.csv("../results/longMod1reg4s1_TW.csv")
longMod1reg4s2_TW<-read.csv("../results/longMod1reg4s2_TW.csv")

# now plot results with typical water consumption at baseline
p1_TW<-
  ggplot(data=longMod1reg4s1_TW %>% filter(metal!="UTAS"), aes(x=term,y=reorder(metal,ChemOrder)))+
  geom_tile(aes(fill=as.factor(dir)))+
  geom_text(aes(label = signif(transformed, 2))) +
  scale_fill_manual(values=c("#f4a582","#92c5de","#ca0020", "#0571b0"),
                    labels=c("negative",
                             "positive",
                             "significantly negative",
                             "significantly positive"))+
  geom_vline(xintercept=c(1.5,2.5,3.5,4.5,5.5,8.5), color ="black", linewidth  = 1.5)+
  scale_x_discrete(labels = c('Smoke','Agro-\nchemicals',"Water","Other\nbeverages"))+
  scale_y_discrete(labels=c("U","Tl","Pb","Mo","Mn","Hg","Cs","Co","Cd","Ba",
                            expression(Sigma*"As"),"AB"))+
  #"AB",expression(Sigma*"As")))+
  theme(axis.text.x = element_text(size=10))+
  theme(axis.text.y = element_text(size=10))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(plot.margin = unit(c(1,1,3,1), "lines")) +
  coord_cartesian(ylim=c(1,12),clip = "off")+
  xlab("")+
  ylab("")+
  theme(panel.border = element_blank(),
        panel.background = element_blank())

p2_TW<-
  ggplot(data=longMod1reg4s2_TW %>% filter(metal!="UTAS"), aes(x=term,y=reorder(metal,ChemOrder)))+
  geom_tile(aes(fill=as.factor(dir)))+
  geom_text(aes(label = signif(transformed, 2))) +
  scale_fill_manual(values=c("#f4a582","#92c5de","#ca0020", "#0571b0"),
                    labels=c("negative",
                             "positive",
                             "significantly negative",
                             "significantly positive"))+
  geom_vline(xintercept=c(8.5,14.5), color ="black", size  = 1.5)+
  scale_x_discrete(labels = c("C1","S1","S2","S3","S4","P1","P2","B1",
                              "RD1","RD5","RD6","RD7","RD3","RD2"))+
  scale_y_discrete(labels=c("U","Tl","Pb","Mo","Mn","Hg","Cs","Co","Cd","Ba",
                            expression(Sigma*"As"),"AB"))+
  #"AB",expression(Sigma*"As")))+
  theme(axis.text.x = element_text(size=10))+
  theme(axis.text.y = element_text(size=10))+
  theme(legend.position = "bottom",legend.justification.bottom = c(0.5,-.2))+
  theme(legend.title = element_blank())+
  theme(plot.margin = unit(c(1.2,1,4,1), "lines")) +
  coord_cartesian(ylim=c(1,12),clip = "off")+
  xlab("")+
  ylab("")+
  theme(panel.border = element_blank(),
        panel.background = element_blank())

p1_TW/p2_TW
#ggsave("..//results/reg4plot_TW.jpg", height=10, width=9, units="in")


