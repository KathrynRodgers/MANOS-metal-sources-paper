# author: KR
# date: 10/23/24
# purpose: make metals and speciated As datasets ready to work with

library(tidyverse)
library(openxlsx)
library(patchwork)

#read in baseline regression results
longMod1reg4s1<-read.csv("../results/longMod1reg4s1.csv")
longMod1reg4s2<-read.csv("../results/longMod1reg4s2.csv")

# make heatmap of reg4 baseline results
DWtext <- textGrob("Drinking water\n    source", gp=gpar(fontsize=10))
kidtext<- textGrob("Kidney function", gp=gpar(fontsize=10))
p1<-
  ggplot(data=longMod1reg4s1 %>% filter(metal!="UTAS"), aes(x=term,y=reorder(metal,ChemOrder)))+
  geom_tile(aes(fill=as.factor(dir)))+
  geom_text(aes(label = signif(transformed, 2))) +
  scale_fill_manual(values=c("#f4a582","#92c5de","#ca0020", "#0571b0"),
                    labels=c("negative",
                             "positive",
                             "significantly negative",
                             "significantly positive"))+
  geom_vline(xintercept=c(1.5,2.5,3.5,4.5,5.5,8.5), color ="black", linewidth  = 1.5)+
  scale_x_discrete(labels = c('Smoke','Agro-\nchemicals','Sea-\nfood',"Water","Other\nbeverages",
                              "Common\nwell","Muni-\ncipal","Common\nmunicipal",
                              "Moderate\nloss","Mild\nloss"))+
  scale_y_discrete(labels=c("U","Tl","Pb","Mo","Mn","Hg","Cs","Co","Cd","Ba",
                            #"AB",expression(Sigma*"As")))+
                            expression(Sigma*"As"),"AB"))+
  theme(axis.text.x = element_text(size=10))+
  theme(axis.text.y = element_text(size=10))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(plot.margin = unit(c(1,1,3,1), "lines")) +
  annotate("segment",x=5.6,xend=8.4,y=-1.5,yend = -1.5)+
  annotate("segment",x=8.6,xend=10.4,y=-1.5,yend = -1.5)+
  annotation_custom(DWtext,xmin=7,xmax=7,ymin=-2.45,ymax=-2.45) + 
  annotation_custom(kidtext,xmin=9.5,xmax=9.5,ymin=-2.45,ymax=-2.45)+
  coord_cartesian(ylim=c(1,12),clip = "off")+
  xlab("")+
  ylab("")+
  theme(panel.border = element_blank(),
        panel.background = element_blank())

EStext <- textGrob("El Salvador", gp=gpar(fontsize=10))
Ntext <- textGrob("Nicaragua", gp=gpar(fontsize=10))
indtext <- textGrob("industry", gp=gpar(fontsize=10))
wstext<- textGrob("worksite", gp=gpar(fontsize=10))
rdtext<- textGrob("residential\ndepartment (RD)", gp=gpar(fontsize=10))
p2<-
  ggplot(data=longMod1reg4s2 %>% filter(metal!="UTAS"), aes(x=term,y=reorder(metal,ChemOrder)))+
  geom_tile(aes(fill=as.factor(dir)))+
  geom_text(aes(label = signif(transformed, 2))) +
  scale_fill_manual(values=c("#f4a582","#92c5de","#ca0020", "#0571b0"),
                    labels=c("negative",
                             "positive",
                             "significantly negative",
                             "significantly positive"))+
  geom_vline(xintercept=c(1.5,5.5,13.5), color ="black", size  = 1.5)+
  scale_x_discrete(labels = c('Nica-\nragua','Corn','Sugar\ncane',"Plan-\ntain","Brick",
                              "C1","S1","S2","S3","S4","P1","P2","B1",
                              "RD1","RD5","RD6","RD7","RD3","RD2"))+
  scale_y_discrete(labels=c("U","Tl","Pb","Mo","Mn","Hg","Cs","Co","Cd","Ba",
                            expression(Sigma*"As"),"AB"))+
  #"AB",expression(Sigma*"As")))+
  theme(axis.text.x = element_text(size=10))+
  theme(axis.text.y = element_text(size=10))+
  theme(legend.position = "bottom",legend.justification.bottom = c(0.5,-.2))+
  theme(legend.title = element_blank())+
  theme(plot.margin = unit(c(1.2,1,4,1), "lines")) +
  annotate("segment",x=5.6,xend=8.4,y=-1,yend = -1)+
  annotate("segment",x=8.6,xend=13.4,y=-1,yend = -1)+
  annotate("segment",x=13.6,xend=17.4,y=-1,yend = -1)+
  annotate("segment",x=17.6,xend=19.4,y=-1,yend = -1)+
  annotation_custom(EStext,xmin=7,xmax=7,ymin=-1.5,ymax=-1.5) + 
  annotation_custom(Ntext,xmin=11,xmax=11,ymin=-1.5,ymax=-1.5) + 
  annotation_custom(EStext,xmin=15.5,xmax=15.5,ymin=-1.5,ymax=-1.5) + 
  annotation_custom(Ntext,xmin=18.5,xmax=18.5,ymin=-1.5,ymax=-1.5) + 
  annotation_custom(indtext,xmin=3.5,xmax=3.5,ymin=13.4,ymax=13.4) + 
  annotation_custom(wstext,xmin=9.5,xmax=9.5,ymin=13.4,ymax=13.4) + 
  annotation_custom(rdtext,xmin=15.5,xmax=15.5,ymin=13.4,ymax=13.4) + 
  coord_cartesian(ylim=c(1,12),clip = "off")+
  xlab("")+
  ylab("")+
  theme(panel.border = element_blank(),
        panel.background = element_blank())

p1/p2
#ggsave("..//results/reg4plot.jpg", height=10, width=9, units="in")


