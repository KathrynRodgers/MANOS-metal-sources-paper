# author: KR
# date: 10/23/24
# purpose: make metals and speciated As datasets ready to work with

library(tidyverse)
library(openxlsx)
library(patchwork)
library(grid)
library(cowplot)
library(scales)

#read in baseline regression results
longMod2regs1<-read.csv("../results/longMod2regs1.csv")
longMod2regs2<-read.csv("../results/longMod2regs2.csv")

#graph forest plot for R1R6 models####
longMod2regs1.1<-longMod2regs1 %>% 
  mutate(metal=case_when(metal=="oAs"~"AB",
                         TRUE~as.character(metal))) %>%  
  filter(metal!="UTAS")

longMod2regs2.1<-longMod2regs2 %>% 
  mutate(metal=case_when(metal=="oAs"~"AB",
                         TRUE~as.character(metal))) %>%  
  filter(metal!="UTAS")

#smoking
Lplot1<-
  ggplot(data=longMod2regs1.1 %>% filter(term=="Smokecurrent1Si") %>% 
           mutate(group="Smoking"), 
         aes(x=reorder(metal,ChemOrder),y=transformed, ymin=lower_CI95, ymax=upper_CI95)) +
  geom_pointrange(size=0.3) + 
  geom_hline(yintercept=0, lty=2) +   # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("") +
  theme_bw()+
  theme(axis.text.y = element_text(size = 7),axis.ticks.y=element_blank(),
        axis.text.x=element_text(angle=90))+
  facet_wrap(~group)

#seafood
Lplot2<-
  ggplot(data=longMod2regs1.1 %>% filter(term=="SeafoodAnyYN1") %>% 
           mutate(group="Seafood"), 
         aes(x=reorder(metal,ChemOrder), y=transformed, ymin=lower_CI95, ymax=upper_CI95)) +
  geom_pointrange(size=0.3) + 
  geom_hline(yintercept=0, lty=2) +   # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("") +
  theme_bw()+
  theme(axis.text.y = element_text(size = 7),axis.ticks.y=element_blank(),
        axis.text.x=element_text(angle=90))+
  facet_wrap(~group)

#water
Lplot4<-
  ggplot(data=longMod2regs1.1 %>% filter(term=="Typical_totalwaterwork") %>% 
           mutate(group="Water"), 
         aes(x=reorder(metal,ChemOrder), y=transformed, ymin=lower_CI95, ymax=upper_CI95)) +
  geom_pointrange(size=0.3) + 
  geom_hline(yintercept=0, lty=2) +   # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("") +
  theme_bw()+
  theme(axis.text.y = element_text(size = 7),axis.ticks.y=element_blank(),
        axis.text.x=element_text(angle=90))+
  facet_wrap(~group)

#not water
Lplot5<-
  ggplot(data=longMod2regs1.1 %>% filter(term=="Typical_totalnotwaterwork") %>% 
           mutate(group="Other beverages"), 
         aes(x=reorder(metal,ChemOrder), y=transformed, ymin=lower_CI95, ymax=upper_CI95)) +
  geom_pointrange(size=0.3) + 
  geom_hline(yintercept=0, lty=2) +   # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("") +
  theme_bw()+
  theme(axis.text.y = element_text(size = 7),axis.ticks.y=element_blank(),
        axis.text.x=element_text(angle=90))+
  facet_wrap(~group)

#drinking water source
longMod2regs1.2<-longMod2regs1.1 %>% 
  filter(term=="DWhome3DWhomecommonwell"|term=="DWhome3DWhomemuni"|
           term=="DWhome3DWhomemunicommon") %>% 
  mutate(term=case_when(term=="DWhome3DWhomecommonwell"~"Common well",
                        term=="DWhome3DWhomemuni"~"Municipal",
                        term=="DWhome3DWhomemunicommon"~"Municipal common"),
         group="Drinking Water Source") %>% 
  mutate(term = factor(term, levels=c("Common well","Municipal common","Municipal")))

Lplot6<-
  ggplot(data=longMod2regs1.2,
         aes(x=reorder(metal,ChemOrder), y=transformed, ymin=lower_CI95, ymax=upper_CI95)) +
  geom_point(aes(color=term))+
  geom_pointrange(aes(color=term),position=position_dodge(width=0.3),size=0.3,show_guide=FALSE) + 
  geom_hline(yintercept=0, lty=2) +   # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("") +
  guides(color=guide_legend(ncol=1))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 7),
        axis.text.x=element_text(angle=90),
        axis.ticks.y=element_blank(),
        legend.position = "bottom",
        legend.title=element_blank(),legend.margin=margin(c(-25,0,0,0)),
        legend.text=element_text(size=7,margin=margin(l=-0.5)),
        legend.key.spacing.y = unit(-0.2, 'cm'))+
  facet_wrap(~group)

#kidney
Lplot7<-
  ggplot(data=longMod2regs1.1 %>% filter(term=="as.factor(kidfunc)1"|term=="as.factor(kidfunc)2") %>% 
           mutate(term=case_when(term=="as.factor(kidfunc)1"~"mild loss",
                                 term=="as.factor(kidfunc)2"~"moderate loss"),
                  group="Kidney function"),
         aes(x=reorder(metal,ChemOrder), y=transformed, ymin=lower_CI95, ymax=upper_CI95)) +
  geom_point(aes(color=term))+
  geom_pointrange(aes(color=term),position=position_dodge(width=0.3),size=0.3,show_guide=FALSE) + 
  geom_hline(yintercept=0, lty=2) +   # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("") +
  guides(color=guide_legend(ncol=1))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 7),
        axis.text.x=element_text(angle=90),
        axis.ticks.y=element_blank(),
        legend.position = "bottom",
        legend.title=element_blank(),legend.margin=margin(c(-30,0,0,0)),
        legend.text=element_text(size=7,margin=margin(l=-0.5)),
        legend.key.spacing.y = unit(-0.2, 'cm'))+
  facet_wrap(~group)

#study visit
Lplot8<-
  ggplot(data=longMod2regs2.1 %>% filter(term=="round") %>% 
           mutate(group="Visit 6"),
         aes(x=reorder(metal,ChemOrder), y=transformed, ymin=lower_CI95, ymax=upper_CI95)) +
  geom_pointrange(size=0.3) + 
  geom_hline(yintercept=0, lty=2) +   # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("") +
  theme_bw()+
  theme(axis.text.y = element_text(size = 7),axis.ticks.y=element_blank(),
        axis.text.x=element_text(angle=90))+
  facet_wrap(~group)

#industry
longMod2regs2.2<-longMod2regs2.1 %>% 
  filter(term=="industrycorn"|term=="industrysugarcane"|term=="d_Indother"|
           term=="industryconstruction") %>% 
  mutate(group="Industry",
         term = factor(term, levels=c("industrycorn","industrysugarcane","d_Indother")))

Lplot9<-
  ggplot(data=longMod2regs2.2,aes(x=reorder(metal,ChemOrder), y=transformed, 
                                  ymin=lower_CI95, ymax=upper_CI95,group=term)) +
  geom_point(aes(color=term),position=position_dodge(width=0.6))+
  geom_pointrange(aes(color=as.factor(term)),position=position_dodge(width=0.6),size=0.3,show_guide=FALSE) +
  scale_color_manual("",values=c("#cab2d6","#33a02c","#6a3d9a"), #"#ffd700","#33a02c","#1f78b4","#e31a1c"
                     labels=c("corn","sugarcane","other"))+
  geom_hline(yintercept=0, lty=2) +   # add a dotted line at x=1 after flip
  coord_flip()+
  #coord_flip(ylim=c(-100,200)) + #clip="off"
  xlab("") + ylab("") +
  guides(color=guide_legend(ncol=1))+
  theme_bw()+
  guides(color=guide_legend(ncol=2))+
  theme(axis.text.y = element_text(size = 7),
        axis.text.x=element_text(angle=90,vjust=-0.25),
        axis.ticks.y=element_blank(),
        legend.position = "bottom",
        legend.title=element_blank(),legend.margin=margin(c(-25,0,0,0)),legend.text=element_text(size=7),
        legend.key.spacing.y = unit(-0.2, 'cm'))+
  facet_wrap(~group)

#worksite
longMod2regs2.3<-longMod2regs2.1 %>% 
  filter(term=="worksiteMA3"|term=="worksiteAZ1"|term=="worksiteAZ2"|
           term=="worksiteOther") %>% 
  mutate(group="Worksite",
         term = factor(term, levels=c("worksiteMA3","worksiteAZ1","worksiteAZ2","worksiteOther")))

Lplot10<-
  ggplot(data=longMod2regs2.3,
         aes(x=reorder(metal,ChemOrder), y=transformed, ymin=lower_CI95, ymax=upper_CI95)) +
  geom_point(aes(color=term),position=position_dodge(width=0.6))+
  geom_pointrange(aes(color=term,group=term),position=position_dodge(width=0.6),size=0.3,
                  show_guide=FALSE) + 
  scale_color_manual("",values=c( "#cab2d6","#b2df8a","#33a02c","#6a3d9a"), #,"#1f78b4","#fdbf6f","#ff7f00", "#e31a1c"),# "#ffd700","#99d8c9","#33a02c", "#a6cee3","#1f78b4","#cab2d6","#6a3d9a", "#e31a1c"
                     labels=c("C1","S1","S2","other"))+
  geom_hline(yintercept=0, lty=2) +   # add a dotted line at x=1 after flip
  #coord_flip()+
  coord_flip(ylim=c(-100,500)) +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("") +
  guides(color=guide_legend(ncol=2))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 7),
        axis.text.x=element_text(angle=90,vjust=-0.25),
        axis.ticks.y=element_blank(),
        legend.position = "bottom",
        legend.title=element_blank(),legend.margin=margin(c(-25,0,0,0)),
        legend.text=element_text(size=7),
        legend.key.spacing.y = unit(-0.2, 'cm'))+
  facet_wrap(~group)

#residential department
longMod2regs2.4<-longMod2regs2.1 %>% 
  filter(term=="Dept4Usulutan"|term=="Dept4SanSalvador"|
           term=="Dept4Ahuachapan"|term=="Dept4Sonsonate") %>% 
  mutate(group="Residential Department",
         term = factor(term, levels=c("Dept4Usulutan","Dept4Sonsonate","Dept4SanSalvador",
                                      "Dept4Ahuachapan")))
Lplot11<-
  ggplot(data=longMod2regs2.4,
         aes(x=reorder(metal,ChemOrder), y=transformed, ymin=lower_CI95, ymax=upper_CI95)) +
  geom_point(aes(color=term),position=position_dodge(width=0.6))+ #,size=0.3
  geom_pointrange(aes(color=term),position=position_dodge(width=0.6),size=0.3,show_guide=FALSE) + 
  scale_color_manual("",values=c( "#cab2d6","#33a02c","#a65628","#ae017e"), 
                     labels=c("RD1","RD5","RD6","RD7"))+
  geom_hline(yintercept=0, lty=2) +   # add a dotted line at x=1 after flip
  coord_flip()+ # flip coordinates (puts labels on y axis)
  #coord_flip(ylim=c(-100,200)) +   #,clip="off"
  xlab("") + ylab("") +
  guides(color=guide_legend(ncol=2))+
  theme_bw()+
  theme(axis.text.y = element_text(size = 7),
        axis.text.x=element_text(angle=90,vjust=-0.25),
        axis.ticks.y=element_blank(),
        legend.position = "bottom",
        strip.text = element_text(hjust=0),
        legend.title=element_blank(),legend.margin=margin(c(-25,0,0,0)),
        legend.text=element_text(size=7),
        legend.key.spacing.y = unit(-0.2, 'cm'))+
  facet_wrap(~group)


(Lplot1|Lplot2|Lplot4|Lplot5)/(Lplot6|Lplot7|Lplot8)/(Lplot9|Lplot10|Lplot11)+
  plot_layout(heights = c(5,5,12))+
  theme(plot.margin = margin(0, 0, 0, 0))+
  plot_annotation('Percent difference (95% CI)',
                  theme=theme(plot.title=element_text(hjust=0.5,vjust=-290)))
#ggsave("..//results/forest6regplot.jpg", height=10, width=8, units="in")

