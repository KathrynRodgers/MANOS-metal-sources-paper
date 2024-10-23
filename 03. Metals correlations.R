# author: KR
# date: 10/23/24
# purpose: make metals and speciated As datasets ready to work with

library(tidyverse)
library(openxlsx)
library(ggcorrplot)

#read in metals and speciated As data
metalsR1R6_q<- read.csv("../results/metalsR1R6_q.csv")
As_IOR1R6_q<- read.csv("../results/As_IOR1R6_q.csv")

# correlations: let's look at metals correlations ####
#bring in As species
allmetalsR1R6<-rbind(metalsR1R6_q %>% select(MANOS_ID,metal,country,round,conc1,osmol,Osmadjconc1),
                     As_IOR1R6_q %>% filter(species!="Total") %>% 
                       rename(metal=species,conc1=As,Osmadjconc1=As_osmadj) %>% 
                       select(MANOS_ID,metal,country,round,conc1,osmol,Osmadjconc1)) %>% 
  mutate(MANOS_ID=as.numeric(MANOS_ID),
         metal=case_when(metal=="As"~"UTAs",
                         metal=="Inorganic"~"sumAs",
                         metal=="Organic"~"AB",
                         TRUE~as.character(metal)))

chemorder<-allmetalsR1R6 %>%
  select(metal) %>%
  unique() %>%
  mutate(ChemOrder=case_when(
    metal=="sumAs"~17,
    metal=="AB"~16,
    metal=="UTAs" ~ 15,
    metal=="Ba" ~ 14,
    metal=="Be" ~ 13,
    metal=="Cd" ~ 12,
    metal=="Co" ~ 11,
    metal=="Cs" ~ 10,
    metal=="Hg" ~ 9,
    metal=="Mn" ~ 8,
    metal=="Mo" ~ 7,
    metal=="Pb" ~ 6,
    metal=="Tb" ~ 5,
    metal=="Sn" ~ 4,
    metal=="Tl" ~ 3,
    metal=="W" ~2,
    metal=="U" ~1))

allmetalsR1R6.1<-allmetalsR1R6 %>% 
  left_join(chemorder,by="metal")

allmetals_wide<-allmetalsR1R6.1 %>% 
  filter(!is.na(osmol)) %>% 
  arrange(ChemOrder) %>% 
  select(MANOS_ID,round,metal,Osmadjconc1) %>% 
  filter(metal!="Be"&metal!="W"&metal!="Sb"&metal!="Sn") %>% 
  unique() %>% 
  pivot_wider(names_from = metal, values_from = Osmadjconc1) %>% 
  na.omit()

metalsR1R6ES_wide<-allmetalsR1R6.1 %>% 
  filter(country=="El Salvador") %>% 
  filter(!is.na(osmol)) %>% 
  arrange(ChemOrder) %>% 
  select(MANOS_ID,round,metal,Osmadjconc1) %>% 
  filter(metal!="Be"&metal!="W"&metal!="Sb"&metal!="Sn") %>% 
  unique() %>% 
  pivot_wider(names_from = metal, values_from = Osmadjconc1) %>% 
  na.omit()

metalsR1_wide<-allmetalsR1R6.1 %>% 
  filter(round==1) %>% 
  filter(!is.na(osmol)) %>% 
  arrange(ChemOrder) %>% 
  select(MANOS_ID,metal,Osmadjconc1) %>% 
  filter(metal!="Be"&metal!="W"&metal!="Sb"&metal!="Sn") %>% 
  unique() %>% 
  pivot_wider(names_from = metal, values_from = Osmadjconc1) %>% 
  na.omit()

metalsR6_wide<-allmetalsR1R6.1 %>% 
  filter(round==6) %>% 
  arrange(ChemOrder) %>% 
  filter(!is.na(osmol)) %>% 
  select(MANOS_ID,metal,Osmadjconc1) %>% 
  filter(metal!="Be"&metal!="W"&metal!="Sb"&metal!="Sn") %>% 
  unique() %>% 
  pivot_wider(names_from = metal, values_from = Osmadjconc1) %>% 
  na.omit()

metalsR1ES_wide<-allmetalsR1R6.1 %>% 
  filter(round==1) %>% 
  filter(country=="El Salvador") %>% 
  filter(!is.na(osmol)) %>% 
  arrange(ChemOrder) %>% 
  select(MANOS_ID,metal,Osmadjconc1) %>% 
  filter(metal!="Be"&metal!="W"&metal!="Sb"&metal!="Sn") %>% 
  unique() %>% 
  pivot_wider(names_from = metal, values_from = Osmadjconc1) %>% 
  na.omit()

chem_corrall <- cor(as.matrix(allmetals_wide %>% select(-MANOS_ID,-round)), method = "kendall") #PREVIOUSLY SPEARMAN
########  PLOT
all<-
  ggcorrplot(chem_corrall, type="lower", lab=TRUE, lab_col = "black", lab_size = 4, digits = 2, 
             outline.col = "white", tl.cex = 10) +
  ggplot2::scale_x_discrete(labels=c("Tl","Pb","Mo","Mn","Hg","Cs","Co","Cd","Ba","UTAs","AB",expression(Sigma*"As")))+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), 
                       space = "Lab", name = "Kendall\nCorrelation")+
  theme(axis.text.x=element_text(size=13, angle=90, vjust=1, hjust=1, 
                                 margin=margin(-3,0,0,0)),
        axis.text.y=element_text(size=13, margin=margin(0,-3,0,0)),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(.2,0,.2,0),"cm"),
        panel.grid.major=element_blank()) +
  ggtitle("El Salvador visits 1 + 6 and Nicaragua visit 1")

chem_corrR1 <- cor(as.matrix(metalsR1_wide %>% select(-MANOS_ID)), method = "kendall") #PREVIOUSLY SPEARMAN
########  PLOT
R1<-
  ggcorrplot(chem_corrR1, type="lower", lab=TRUE, lab_col = "black", lab_size = 3, digits = 2, 
             outline.col = "white", tl.cex = 10) +
  ggplot2::scale_x_discrete(labels=c("Tl","Pb","Mo","Mn","Hg","Cs","Co","Cd","Ba","UTAs","AB",expression(Sigma*"As")))+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), 
                       space = "Lab", name = "Kendall\nCorrelation")+
  theme(axis.text.x=element_text(size=10, angle=90, vjust=1, hjust=1, 
                                 margin=margin(-3,0,0,0)),
        axis.text.y=element_text(size=10, margin=margin(0,-3,0,0)),
        legend.position="none",
        plot.title = element_text(hjust = 0.5),
        panel.grid.major=element_blank()) +
  ggtitle("El Salvador and Nicaragua visit 1")
#ggsave("..//results/R1corrplot.jpg",height=7, width=7, units="in")

chem_corrR1R6ES <- cor(as.matrix(metalsR1R6ES_wide %>% select(-MANOS_ID,-round)), method = "kendall") #PREVIOUSLY SPEARMAN
########  PLOT
ESR1R6<-
  ggcorrplot(chem_corrR1R6ES, type="lower", lab=TRUE, lab_col = "black", lab_size = 3, digits = 2, 
             outline.col = "white", tl.cex = 10) +
  ggplot2::scale_x_discrete(labels=c("Tl","Pb","Mo","Mn","Hg","Cs","Co","Cd","Ba","UTAs","AB",expression(Sigma*"As")))+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), 
                       space = "Lab", name = "Kendall\nCorrelation")+
  theme(axis.text.x=element_text(size=10, angle=90, vjust=1, hjust=1, 
                                 margin=margin(-3,0,0,0)),
        axis.text.y=element_text(size=10, margin=margin(0,-3,0,0)),
        #legend.position="none",
        plot.title = element_text(hjust = 0.5),
        panel.grid.major=element_blank())+
  ggtitle("El Salvador visits 1 + 6")

(R1|ESR1R6)/all
(R1|ESR1R6)/all+plot_layout(heights = c(5, 7))
#ggsave("..//results/ESR1R6corrplot.jpg",height=9, width=12, units="in",dpi=300)

(R1|ESR1R6)/all+plot_layout(heights = c(5, 7),guides="collect") &
  theme(legend.position = "right")


chem_corrR1ES <- cor(as.matrix(metalsR1ES_wide %>% select(-MANOS_ID)), method = "kendall") #PREVIOUSLY SPEARMAN
########  PLOT
ESR1<-
  ggcorrplot(chem_corrR1ES, type="lower", lab=TRUE, lab_col = "black", lab_size = 4, digits = 2, 
             outline.col = "white", tl.cex = 10) +
  ggplot2::scale_x_discrete(labels=c("Tl","Pb","Mo","Mn","Hg","Cs","Co","Cd","Ba","UTAs","AB",expression(Sigma*"As")))+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), 
                       space = "Lab", name = "Kendall\nCorrelation")+
  theme(axis.text.x=element_text(size=13, angle=45, vjust=1, hjust=1, 
                                 margin=margin(-3,0,0,0)),
        axis.text.y=element_text(size=13, margin=margin(0,-3,0,0)),
        legend.position="none",
        plot.title = element_text(hjust = 0.5),
        panel.grid.major=element_blank())+
  ggtitle("El Salvador visit 1")
#ggsave("..//results/R1EScorrplot.jpg",height=7, width=7, units="in")

chem_corrR6 <- cor(as.matrix(metalsR6_wide %>% select(-MANOS_ID)), method = "kendall") #PREVIOUSLY SPEARMAN
########  PLOT
ESR6<-
  ggcorrplot(chem_corrR6, type="lower", lab=TRUE, lab_col = "black", lab_size = 4, digits = 2, 
             outline.col = "white", tl.cex = 10) +
  ggplot2::scale_x_discrete(labels=c("Tl","Pb","Mo","Mn","Hg","Cs","Co","Cd","Ba","UTAs","AB",expression(Sigma*"As")))+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), 
                       space = "Lab", name = "Kendall\nCorrelation")+
  theme(axis.text.x=element_text(size=13, angle=45, vjust=1, hjust=1, 
                                 margin=margin(-3,0,0,0)),
        axis.text.y=element_text(size=13, margin=margin(0,-3,0,0)),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major=element_blank()) +
  ggtitle("El Salvador visit 6")
#ggsave("..//results/R6corrplot.jpg",height=7, width=7, units="in")



