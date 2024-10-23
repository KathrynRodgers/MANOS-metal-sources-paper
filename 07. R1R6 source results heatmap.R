# author: KR
# date: 10/23/24
# purpose: make metals and speciated As datasets ready to work with

library(tidyverse)
library(openxlsx)
library(patchwork)

#read in baseline regression results
longMod2regs1<-read.csv("../results/longMod2regs1.csv")
longMod2regs2<-read.csv("../results/longMod2regs2.csv")

DWtext <- textGrob("Drinking water\n   source", gp=gpar(fontsize=10))
kidtext<- textGrob("Kidney function", gp=gpar(fontsize=10))
p3<-
  ggplot(data=longMod2regs1 %>% filter(metal!="UTAS"), aes(x=term,y=reorder(metal,ChemOrder)))+
  geom_tile(aes(fill=as.factor(dir)))+
  geom_text(aes(label = signif(transformed, 2))) +
  scale_fill_manual(values=c("#f4a582","#92c5de","#ca0020", "#0571b0"),
                    labels=c("negative",
                             "positive",
                             "significantly negative",
                             "significantly positive"))+
  geom_vline(xintercept=c(1.5,2.5,3.5,4.5,7.5), color ="black", linewidth  = 1.5)+
  scale_x_discrete(labels = c('Smoke','Sea-\nfood',"Water","Other\nbeverage",
                              "Common\nwell","Muni-\ncipal","Common\nmunicipal",
                              "Moderate\nloss","Mild\nloss"))+
  scale_y_discrete(labels=c("U","Tl","Pb","Mo","Mn","Hg","Cs","Co","Cd","Ba",
                            expression(Sigma*"As"),"AB"))+
  #"UTAs","AB",expression(Sigma*"As")))+
  theme(axis.text.x = element_text(size=10))+
  theme(axis.text.y = element_text(size=10))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(plot.margin = unit(c(1,1,3,1), "lines")) +
  annotate("segment",x=4.6,xend=7.4,y=-1.5,yend = -1.5)+
  annotate("segment",x=7.6,xend=9.4,y=-1.5,yend = -1.5)+
  annotation_custom(DWtext,xmin=6,xmax=6,ymin=-2.45,ymax=-2.45) + 
  annotation_custom(kidtext,xmin=8.5,xmax=8.5,ymin=-2.45,ymax=-2.45)+
  coord_cartesian(ylim=c(1,12),clip = "off")+
  xlab("")+
  ylab("")+
  theme(panel.border = element_blank(),
        panel.background = element_blank())

indtext <- textGrob("industry", gp=gpar(fontsize=10))
wstext<- textGrob("worksite", gp=gpar(fontsize=10))
rdtext<- textGrob("residential\ndepartment (RD)", gp=gpar(fontsize=10))
p4<-
  ggplot(data=longMod2regs2 %>% filter(metal!="UTAS"), aes(x=term,y=reorder(metal,ChemOrder)))+
  geom_tile(aes(fill=as.factor(dir)))+
  geom_text(aes(label = signif(transformed, 2))) +
  scale_fill_manual(values=c("#f4a582","#92c5de","#ca0020", "#0571b0"),
                    labels=c("negative",
                             "positive",
                             "significantly negative",
                             "significantly positive"))+
  geom_vline(xintercept=c(1.5,4.5,8.5), color ="black", size  = 1.5)+
  scale_x_discrete(labels = c('Visit','Corn','Sugar\ncane','other',
                              "C1","S1","S2","other",
                              "RD1","RD5","RD6","RD7"))+
  scale_y_discrete(labels=c("U","Tl","Pb","Mo","Mn","Hg","Cs","Co","Cd","Ba",
                            expression(Sigma*"As"),"AB"))+
  #"UTAs","AB",expression(Sigma*"As")))+
  theme(axis.text.x = element_text(size=10))+
  theme(axis.text.y = element_text(size=10))+
  theme(legend.position = "bottom",legend.justification.bottom = c(0.5,-.2))+
  theme(legend.title = element_blank())+
  theme(plot.margin = unit(c(1.2,1,4,1), "lines")) +
  annotation_custom(indtext,xmin=3,xmax=3,ymin=13.4,ymax=13.4) + 
  annotation_custom(wstext,xmin=6.5,xmax=6.5,ymin=13.4,ymax=13.4) + 
  annotation_custom(rdtext,xmin=10.5,xmax=10.5,ymin=13.4,ymax=13.4) + 
  coord_cartesian(ylim=c(1,12),clip = "off")+
  xlab("")+
  ylab("")+
  theme(panel.border = element_blank(),
        panel.background = element_blank())

p3/p4
#ggsave("..//results/regR1R6plot.jpg", height=10, width=9, units="in")


