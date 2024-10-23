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
longMod1reg4s1<-read.csv("../results/longMod1reg4s1.csv")
longMod1reg4s2<-read.csv("../results/longMod1reg4s2.csv")

#graph forest plot for baseline models #
longMod1reg4s1.1<-longMod1reg4s1 %>% 
  mutate(metal=case_when(metal=="oAs"~"AB",
                         TRUE~as.character(metal))) %>%  
  filter(metal!="UTAS")

longMod1reg4s2.1<-longMod1reg4s2 %>% 
  mutate(metal=case_when(metal=="oAs"~"AB",
                         TRUE~as.character(metal))) %>%  
  filter(metal!="UTAS")

#smoking
plot1<-
  ggplot(data=longMod1reg4s1.1 %>% filter(term=="Smokecurrent1Si") %>% 
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
plot2<-
  ggplot(data=longMod1reg4s1.1 %>% filter(term=="SeafoodAnyYN") %>% 
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

#agrochemicals
plot3<-
  ggplot(data=longMod1reg4s1.1 %>% filter(term=="PestYN") %>% 
           mutate(group="Agrochemical"), 
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
plot4<-
  ggplot(data=longMod1reg4s1.1 %>% filter(term=="Totalwaterwork") %>% 
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
plot5<-
  ggplot(data=longMod1reg4s1.1 %>% filter(term=="Totalnotwaterwork") %>% 
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
longMod1reg4s1.2<-longMod1reg4s1.1 %>% 
  filter(term=="DWhome3DWhomecommonwell"|term=="DWhome3DWhomemuni"|
           term=="DWhome3DWhomemunicommon") %>% 
  mutate(term=case_when(term=="DWhome3DWhomecommonwell"~"Common well",
                        term=="DWhome3DWhomemuni"~"Municipal",
                        term=="DWhome3DWhomemunicommon"~"Municipal common"),
         group="Drinking Water Source") %>% 
  mutate(term = factor(term, levels=c("Common well","Municipal common","Municipal")))

plot6<-
  ggplot(data=longMod1reg4s1.2,
         aes(x=reorder(metal,ChemOrder), y=transformed, ymin=lower_CI95, ymax=upper_CI95)) +
  geom_point(aes(color=term),position=position_dodge(width=0.3))+
  geom_pointrange(aes(color=term),position=position_dodge(width=0.3),size=0.3,show_guide=FALSE) + 
  geom_hline(yintercept=0, lty=2) +   # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("") +
  theme_bw()+
  theme(axis.text.y = element_text(size = 7),axis.ticks.y=element_blank(),
        legend.position = "bottom",axis.text.x=element_text(angle=90),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.title=element_blank(),legend.margin=margin(c(-25,0,0,0)),
        legend.text=element_text(size=7,margin=margin(l=-.05)),
        legend.key.spacing.y = unit(-0.2, 'cm'))+
  guides(color=guide_legend(ncol=1))+
  facet_wrap(~group)

#kidney
plot7<-
  ggplot(data=longMod1reg4s1.1 %>% filter(term=="as.factor(kidfunc)1"|term=="as.factor(kidfunc)2") %>% 
           mutate(term=case_when(term=="as.factor(kidfunc)1"~"mild loss",
                                 term=="as.factor(kidfunc)2"~"moderate loss"),
                  group="Kidney function"),
         aes(x=reorder(metal,ChemOrder), y=transformed, ymin=lower_CI95, ymax=upper_CI95)) +
  geom_point(aes(color=term),position=position_dodge(width=0.3))+
  geom_pointrange(aes(color=term),position=position_dodge(width=0.3),size=0.3,show_guide=FALSE) + 
  geom_hline(yintercept=0, lty=2) +   # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("") +
  theme_bw()+
  theme(axis.text.y = element_text(size = 7),axis.ticks.y=element_blank(),
        legend.position = "bottom",axis.text.x=element_text(angle=90),
        legend.title=element_blank(),legend.margin=margin(c(-35,0,0,0)),
        legend.text=element_text(size=7,margin=margin(l=-.05)),
        legend.key.spacing.y = unit(-0.2, 'cm'))+
  guides(color=guide_legend(ncol=1))+
  facet_wrap(~group)

#country
plot8<-
  ggplot(data=longMod1reg4s2.1 %>% filter(term=="countryNicaragua") %>% 
           mutate(group="Country"),
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
longMod1reg4s2.2<-longMod1reg4s2.1 %>% 
  filter(term=="industrycorn"|term=="industrysugarcane"|term=="industryplantain"|
           term=="industryconstruction"|term=="industryzbrick") %>% 
  mutate(group="Industry",
         term = factor(term, levels=c("industrycorn","industrysugarcane","industryplantain",
                                      "industryzbrick")))
p9<-
  ggplot(data=longMod1reg4s2.2,aes(x=reorder(metal,ChemOrder), y=transformed, 
                                   ymin=lower_CI95, ymax=upper_CI95,group=term)) +
  geom_point(aes(color=term),position=position_dodge(width=0.6))+
  geom_pointrange(aes(color=as.factor(term)),position=position_dodge(width=0.6),size=0.3,
                  show_guide=FALSE) +
  scale_color_manual("",values=c("#cab2d6","#33a02c","#ff7f00","#e31a1c"), 
                     labels=c("corn","sugarcane","plantain","brick"))+
  geom_hline(yintercept=0, lty=2) +   # add a dotted line at x=1 after flip
  #coord_flip()+
  coord_flip(ylim=c(-100,200)) + #clip="off"
  xlab("") + ylab("") +
  theme_bw()+
  guides(color=guide_legend(ncol=2))+
  theme(axis.text.y = element_text(size = 7),
        axis.text.x=element_text(angle=90,vjust=-0.25),
        axis.ticks.y=element_blank(),
        legend.position = "bottom",
        legend.title=element_blank(),legend.margin=margin(c(-25,0,0,0)),
        legend.text=element_text(size=7,margin=margin(l=-.05)),
        legend.key.spacing.y = unit(-0.2, 'cm'),
        legend.key.spacing.x= unit(-0.05,"cm"),
        panel.spacing = unit(0,"lines"),
        plot.margin=margin(r=0,unit="cm"))+
  facet_wrap(~group)

p9.1<-
  ggplot(data=longMod1reg4s2.1 %>% filter(term=="industrycorn"|term=="industrysugarcane"|term=="industryplantain"|
                                            term=="industryconstruction"|term=="industryzbrick") %>% 
           mutate(group=""),
         aes(x=reorder(metal,ChemOrder), y=transformed, ymin=lower_CI95, ymax=upper_CI95)) +
  geom_point(aes(color=term),position=position_dodge(width=0.6),size=0.3)+
  geom_segment(aes(color=term,group=term,y=lower_CI95,yend=upper_CI95),
               position=position_dodge(width=0.6),size=0.3,show_guide=FALSE)+
  scale_color_manual("",values=c("#cab2d6","#33a02c","#ff7f00","#e31a1c"),
                     labels=c("corn","sugarcane","plantain","brick"))+
  geom_vline(xintercept=0.4)+
  #coord_flip() +  # flip coordinates (puts labels on y axis)
  coord_flip(ylim=c(1400,18500)) + #clip="off"
  geom_hline(yintercept=19300)+
  xlab("") + ylab("") +
  theme_bw()+
  theme(axis.text.y = element_blank(),
        axis.text.x=element_text(angle=90),
        axis.ticks.y=element_blank(),
        legend.position = "none",
        panel.border = element_blank(),
        panel.spacing = unit(0,"lines"),
        plot.margin=margin(l=-0.5,unit="cm"))+
  facet_wrap(~group)

plot9<-plot_grid(p9,p9.1,rel_widths = c(7,3),align="h",axis="r")
plot9

#worksite
longMod1reg4s2.3<-longMod1reg4s2.1 %>% 
  filter(term=="worksiteMA3"|term=="worksiteAZ1"|term=="worksiteAZ2"|
           term=="worksiteAZ5"|term=="worksiteAZ7"|term=="worksitePL1"|
           term=="worksitePL9"|term=="worksiteLA8") %>% 
  mutate(group="Worksite",
         term = factor(term, levels=c("worksiteMA3","worksiteAZ1","worksiteAZ2","worksiteAZ5",
                                      "worksiteAZ7","worksitePL1","worksitePL9","worksiteLA8")))
p10<-
  ggplot(data=longMod1reg4s2.3,
         aes(x=reorder(metal,ChemOrder), y=transformed, ymin=lower_CI95, ymax=upper_CI95)) +
  geom_point(aes(color=term),position=position_dodge(width=0.6))+
  geom_pointrange(aes(color=term,group=term),position=position_dodge(width=0.6),size=0.3,
                  show_guide=FALSE) + 
  scale_color_manual("",values=c( "#cab2d6","#b2df8a","#33a02c","#a6cee3","#1f78b4","#fdbf6f","#ff7f00", "#e31a1c"),
                     labels=c("C1","S1","S2","S3","S4","S5","P1","P2","B1"))+
  geom_hline(yintercept=0, lty=2) +   # add a dotted line at x=1 after flip
  #coord_flip()+
  coord_flip(ylim=c(-100,300)) +  # flip coordinates (puts labels on y axis)
  xlab("") + ylab("") +
  theme_bw()+
  theme(axis.text.y = element_text(size = 7),
        axis.text.x=element_text(angle=90,vjust=-0.25),
        axis.ticks.y=element_blank(),
        legend.position = "bottom",
        legend.title=element_blank(),legend.margin=margin(c(-25,0,0,0)),
        legend.text=element_text(size=7,margin=margin(c(t=0,r=-1,b=0,l=-3))),
        legend.key.spacing.y = unit(-0.2, 'cm'),
        legend.key.spacing.x= unit(-0.07,"cm"),
        panel.spacing = unit(0,"lines"),
        plot.margin=margin(r=0,unit="cm"))+
  facet_wrap(~group)

p10.1<-
  ggplot(data=longMod1reg4s2.3 %>% mutate(group=""),
         aes(x=reorder(metal,ChemOrder), y=transformed, ymin=lower_CI95, ymax=upper_CI95)) +
  geom_point(aes(color=term))+
  geom_pointrange(aes(color=term,group=term),position=position_dodge(width=0.6),size=0.3,
                  show_guide=FALSE) + 
  scale_color_manual("",values=c( "#cab2d6","#b2df8a","#33a02c","#a6cee3","#1f78b4","#fdbf6f","#ff7f00", "#e31a1c"),
                     labels=c("C1","S1","S2","S3","S4","S5","P1","P2","B1"))+
  geom_vline(xintercept=0.4)+
  #coord_flip() +  # flip coordinates (puts labels on y axis)
  coord_flip(ylim=c(500,3700)) +
  geom_hline(yintercept=3863)+
  xlab("") + ylab("") +
  theme_bw()+
  theme(axis.text.y = element_blank(),
        axis.text.x=element_text(angle=90),
        axis.ticks.y=element_blank(),
        legend.position = "none",
        panel.border = element_blank(),
        panel.spacing = unit(0,"lines"),
        plot.margin=margin(l=-0.5,unit="cm"))+
  facet_wrap(~group)

plot10<-plot_grid(p10,p10.1,rel_widths = c(7,3),align="h",axis="r")
plot10

#residential department
longMod1reg4s2.4<-longMod1reg4s2.1 %>% 
  filter(term=="Dept4Usulutan"|term=="Dept4SanSalvador"|
           term=="Dept4Ahuachapan"|term=="Dept4Sonsonate"|
           term=="Dept4Chinandega"|term=="Dept4Leon") %>% 
  mutate(group="Residential Department",
         term = factor(term, levels=c("Dept4Usulutan","Dept4Sonsonate","Dept4SanSalvador","Dept4Ahuachapan",
                                      "Dept4Chinandega","Dept4Leon")))
p11<-
  ggplot(data=longMod1reg4s2.4,
         aes(x=reorder(metal,ChemOrder), y=transformed, ymin=lower_CI95, ymax=upper_CI95)) +
  geom_point(aes(color=term),position=position_dodge(width=0.6))+ #,size=0.3
  geom_pointrange(aes(color=term),position=position_dodge(width=0.6),size=0.3,show_guide=FALSE) + 
  scale_color_manual("",values=c( "#cab2d6","#33a02c","#a65628","#ae017e","#b2df8a", "#ff7f00"),
                     labels=c("RD1","RD5","RD6","RD7","RD3","RD2"))+
  geom_hline(yintercept=0, lty=2) +   # add a dotted line at x=1 after flip
  #coord_flip()+ # flip coordinates (puts labels on y axis)
  coord_flip(ylim=c(-100,200)) +   #,clip="off"
  xlab("") + ylab("") +
  facet_wrap(~group)+
  theme_bw()+
  theme(axis.text.y = element_text(size = 7),
        axis.text.x=element_text(angle=90,vjust=-0.25),
        axis.ticks.y=element_blank(),
        legend.position = "bottom",
        strip.text = element_text(hjust=0),
        legend.title=element_blank(),legend.margin=margin(c(-25,0,0,0)),
        legend.text=element_text(size=7,margin=margin(l=-.05)),
        legend.key.spacing.y = unit(-0.2, 'cm'),
        legend.key.spacing.x= unit(-0.05,"cm"),
        panel.spacing = unit(0,"lines"),
        plot.margin=margin(r=0,unit="cm"),
        legend.spacing.y = unit(0, "pt"))

p11.1<-
  ggplot(data=longMod1reg4s2.4 %>% mutate(group=""),
         aes(x=reorder(metal,ChemOrder), y=transformed, ymin=lower_CI95, ymax=upper_CI95)) +
  geom_point(aes(color=term),position=position_dodge(width=0.6))+
  geom_pointrange(aes(color=term),position=position_dodge(width=0.6),size=0.3,show_guide=FALSE) + 
  scale_color_manual("",values=c( "#cab2d6","#33a02c","#a65628","#ae017e","#b2df8a", "#ff7f00"),
                     labels=c("RD1","RD5","RD6","RD7","RD3","RD2"))+
  #coord_flip() +  # flip coordinates (puts labels on y axis)
  geom_vline(xintercept=0.4)+ 
  coord_flip(ylim=c(300,1300))+ #,clip="off"
  geom_hline(yintercept=1350)+
  xlab("") + ylab("") +
  facet_wrap(~group)+
  theme_bw()+
  theme(axis.text.y = element_blank(),
        axis.text.x=element_text(angle=90),
        axis.ticks.y=element_blank(),
        legend.position = "none",
        panel.border=element_blank(),
        panel.spacing = unit(0,"lines"),
        plot.margin=margin(l=-0.5,unit="cm"))

plot11<-plot_grid(p11,p11.1,rel_widths = c(7.5,2.5),align="h",axis="r")
plot11

(plot1|plot2|plot3|plot4)/(plot5|plot6|plot7|plot8)/(plot9|plot10|plot11) +
  plot_layout(heights = c(5,5,12))+
  theme(plot.margin = margin(0, 0, 0, 0))+
  plot_annotation('Percent difference (95% CI)',
                  theme=theme(plot.title=element_text(hjust=0.5,vjust=-290)))
#ggsave("..//results/forestreg4plot.jpg", height=10, width=8, units="in")

