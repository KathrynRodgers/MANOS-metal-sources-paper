# author: KR
# date: 10/23/24
# purpose: make metals and speciated As datasets ready to work with

library(tidyverse)
library(openxlsx)
library(RNHANES)
library(foreign)
library(haven)
library(patchwork)
library(ggpubr)

#read in metals and speciated As data
metalsR1R6_q<- read.csv("../results/metalsR1R6_q.csv")
As_IOR1R6_q<- read.csv("../results/As_IOR1R6_q.csv")
AsR1R6_q<- read.csv("../results/AsR1R6_q.csv")

# make summary stats table for metals, Table 3 in paper
metals.sumstats<-metalsR1R6 %>% 
  group_by(metal,round,country) %>% 
  summarise(
    N=length(unique(MANOS_ID)),
    DF=signif(length(unique(MANOS_ID[DFlag==1]))/N*100), 
    min=signif(min(conc1, na.rm = TRUE),4),
    min_osm=signif(min(Osmadjconc1, na.rm = TRUE),4), #osm normalized with ratio method
    min_osm2=signif(min(Osmadjconc2, na.rm = TRUE),4), #osm normalized with division method
    mean=signif(mean(conc1),4),
    mean_osm=signif(mean(Osmadjconc1,na.rm = TRUE),4),
    mean_osm2=signif(mean(Osmadjconc2,na.rm = TRUE),4),
    SD=signif(sd(conc1),4),
    SD_osm=signif(sd(Osmadjconc1,na.rm = TRUE),4),
    SD_osm2=signif(sd(Osmadjconc2,na.rm = TRUE),4),
    median=signif(median(conc1),4),
    median_osm=signif(median(Osmadjconc1,na.rm = TRUE),4),
    median_osm2=signif(median(Osmadjconc2,na.rm = TRUE),4),
    GM = signif(exp(mean(log(conc1))),4), 
    GM_osm = signif(exp(mean(log(Osmadjconc1))),4), 
    GM_osm2 = signif(exp(mean(log(Osmadjconc2))),4),
    P95=signif(quantile(conc1, p=0.95, na.rm=TRUE),4),
    P95_osm=signif(quantile(Osmadjconc1, p=0.95, na.rm=TRUE),4),
    P95_osm2=signif(quantile(Osmadjconc2, p=0.95, na.rm=TRUE),4),
    max=signif(max(conc1, na.rm=TRUE),4),
    max_osm=signif(max(Osmadjconc1, na.rm=TRUE),4),
    max_osm2=signif(max(Osmadjconc2, na.rm=TRUE),4),
    LOD=mean(LOD), # this is the average of LODs between batches
  ) %>% 
  unique() %>% 
  mutate(metal_eng=case_when(
    metal=="As"~"Arsenic",
    metal=="Sb"~"Antimony",
    metal=="Ba"~"Barium",
    metal=="Be"~"Beryllium",
    metal=="Cs"~"Cesium",
    metal=="Co"~"Cobalt",
    metal=="Cd"~"Cadmium",
    metal=="Pb"~"Lead",
    metal=="Mn"~"Manganese",
    metal=="Hg"~"Mercury",
    metal=="Mo"~"Molybdenum",
    metal=="Tl"~"Thallium",
    metal=="Sn"~"Tin",
    metal=="W"~"Tungsten",
    metal=="U"~"Uranium",
    TRUE~as.character(metal)
  ))

# sum stats for all forms of Arsenic ####
sumstats_As<-AsR1R6 %>% 
  group_by(As_form,round,country) %>% 
  summarise(
    N=length(unique(MANOS_ID)),
    D=sum(DFlag==1),
    DF=D/N*100,
    min=signif(min(conc1, na.rm = TRUE),4),
    min_osm=signif(min(Osmadjconc1, na.rm = TRUE),4),
    mean=signif(mean(conc1,na.rm=TRUE),4),
    mean_osm=signif(mean(Osmadjconc1,na.rm = TRUE),4),
    SD=signif(sd(conc1,na.rm=TRUE),4),
    SD_osm=signif(sd(Osmadjconc1,na.rm=TRUE),4),
    median=signif(median(conc1,na.rm=TRUE),4),
    median_osm=signif(median(Osmadjconc1,na.rm=TRUE),4),
    GM = signif(exp(mean(log(conc1))),4), 
    P95=signif(quantile(conc1, p=0.95, na.rm=TRUE),4),
    P95_osm=signif(quantile(Osmadjconc1,p=0.95,na.rm=TRUE),4),
    max=signif(max(conc1, na.rm=TRUE),4),
    max_osm=signif(max(Osmadjconc1,na.rm = TRUE),4),
    MDL=mean(MDL)
  ) %>% 
  unique()

#### NHANES: now want to read in NHANES data for comparisons ####
Astot.nhanes<-read.xport("data/P_UTAS.XPT") %>% 
  #URXUAS = Urinary arsenic, total (ug/L)
  select(SEQN,URXUAS) %>% 
  rename(conc.ug.L = URXUAS) %>% 
  add_column(metal="Arsenic")

As.nhanes<-read.xport("data/P_UAS.XPT") %>% 
  #URXURB=arsenobetaine, URXUDMA=DMA, URXUMMA=MMA
  select(SEQN,URXUAB,URXUDMA,URXUMMA) %>% 
  rename(AsFront=URXUAB,
         DMA=URXUDMA,
         MA=URXUMMA) %>% 
  pivot_longer(cols=c(AsFront:MA),
               names_to = "As_form",values_to = "conc")

Hg.nhanes<-read.xport("data/P_UHG.XPT") %>% 
  #URXUHG = mercury, urine (ng/mL) -- same as ug/L
  select(SEQN,URXUHG) %>% 
  rename(conc.ug.L=URXUHG) %>% 
  add_column(metal="Mercury")

metals.nhanes<- read.xport("data/P_UM.XPT") %>% 
  #URXUBA = barium, urine (ug/L)
  #URXUCD = cadmium, urine (ug/L)
  #URXUCO = cobalt, urine (ug/L)
  #URXUCS = cesium, urine (ug/L)
  #URXUMO = molybdenum, urine (ug/L)
  #URXUMN = manganese, urine (ug/L)
  #URXUPB = lead, urine (ug/L)
  #URXUSB = antimony, urine (ug/L)
  #URXUSN = tin, urine (ug/L)
  #URXUTL = thallium, urine (ug/L)
  #URXUTU = tungsten, urine (ug/L)
#URXUUR = uranium, urine (ug/L)
select(SEQN,URXUBA,URXUCD,URXUCO,URXUCS,URXUCS,URXUMO,URXUMN,URXUPB,URXUSB,URXUSN,URXUTL,URXUTU) %>% 
  rename(Barium=URXUBA,
         Cadmium=URXUCD,
         Cobalt=URXUCO,
         Cesium=URXUCS,
         Molybdenum=URXUMO,
         Manganese=URXUMN,
         Lead=URXUPB,
         Antimony=URXUSB,
         Tin=URXUSN,
         Thallium=URXUTL,
         Tungsten=URXUTU) %>% 
  pivot_longer(cols = Barium:Tungsten, names_to = "metal",values_to = "conc.ug.L")

osm.nhanes<-read.xport("data/P_BIOPRO.XPT") %>% 
  #LBXSOSSI = osmolality mmol/Kg males and females 12-150 years of age
  select(SEQN,LBXSOSSI) %>% 
  rename(osm_mmol.Kg=LBXSOSSI) %>% 
  add_column(osm="osm")

nhanes.demo<-read.xport("data/P_DEMO.XPT") %>% 
  select(SEQN,RIAGENDR,RIDAGEYR,RIDRETH1) %>% 
  rename(sex=RIAGENDR,
         age=RIDAGEYR,
         eth=RIDRETH1) 

#join multiple df together
allmetals.nhanes3<-rbind(Astot.nhanes,Hg.nhanes)
allmetals.nhanes2<-rbind(allmetals.nhanes3,metals.nhanes)
allmetals.nhanes<-left_join(allmetals.nhanes2,osm.nhanes,by="SEQN") %>% 
  left_join(.,{nhanes.demo},by="SEQN") %>% 
  filter(sex==1) %>% #only want males
  filter(age<46&age>17) %>%  #want 18-45
  filter(eth==1|eth==2) %>%  #want mexican american or other hispanic
  #Zoe calculated osm-normalized concs with metals * (490/Osmolality). 490 must be conversion factor, need to figure out
  #median osm_mml.Kg=277
  mutate(conc.osm=ifelse(!is.na(osm),(conc.ug.L*(median(osm_mmol.Kg, na.rm=TRUE)/osm_mmol.Kg)),NA)) %>% 
  mutate(metal=case_when(
    metal=="Arsenic"~"As",
    metal=="Antimony"~"Sb",
    metal=="Barium"~"Ba",
    metal=="Beryllium"~"Be",
    metal=="Cesium"~"Cs",
    metal=="Cobalt"~"Co",
    metal=="Cadmium"~"Cd",
    metal=="Lead"~"Pb",
    metal=="Manganese"~"Mn",
    metal=="Mercury"~"Hg",
    metal=="Molybdenum"~"Mo",
    metal=="Thallium"~"Tl",
    metal=="Tin"~"Sn",
    metal=="Tungsten"~"W",
    #metal=="Uranium"~"U",
    TRUE~as.character(metal)
  )) %>% 
  mutate(source="NHANES")

# speciated As df
As.nhanes1<-left_join(As.nhanes,osm.nhanes,by="SEQN") %>% 
  left_join(.,{nhanes.demo},by="SEQN") %>% 
  filter(sex==1) %>% #only want males
  filter(age<46&age>17) %>%  #want 18-45
  filter(eth==1|eth==2) %>%  #want mexican american or other hispanic
  #Zoe calculated osm-normalized concs with metals * (490/Osmolality). 490 must be conversion factor, need to figure out
  #median osm_mml.Kg=277
  mutate(conc.osm=ifelse(!is.na(osm),(conc*(median(osm_mmol.Kg, na.rm=TRUE)/osm_mmol.Kg)),NA)) %>% 
  mutate(source="NHANES")

#make df with inorganic As, sum of DMA and MA
As.nhanes2<-As.nhanes1 %>% 
  group_by(SEQN) %>% 
  mutate(As_I = sum(conc[As_form=="DMA"]+conc[As_form=="MA"]),
         osm_AsIadj1= sum(conc.osm[As_form=="DMA"]+conc.osm[As_form=="MA"],na.rm = TRUE)) %>% 
  mutate(As_form=ifelse(!is.na(As_form),"AsI",As_form)) %>% 
  select(-conc,-conc.osm) %>% 
  rename(conc=As_I,conc.osm=osm_AsIadj1) %>% 
  distinct()
#rbind them together
As.nhanes<-rbind(As.nhanes1,As.nhanes2)

nhanes.sumstats<-allmetals.nhanes %>% 
  group_by(metal) %>% 
  summarise(
    min_osm=min(conc.osm, na.rm = TRUE),
    mean_osm=mean(conc.osm, na.rm=TRUE),
    median_osm=median(conc.osm,na.rm=TRUE),
    #GM = exp(mean(log(conc.osm))), 
    P95_osm=quantile(conc.osm, p=0.95, na.rm=TRUE),
    max_osm=max(conc.osm, na.rm=TRUE),
    min=min(conc.ug.L, na.rm = TRUE),
    mean=mean(conc.ug.L, na.rm=TRUE),
    median=median(conc.ug.L,na.rm=TRUE),
    #GM = exp(mean(log(conc.ug.L))), 
    P95=quantile(conc.ug.L, p=0.95, na.rm=TRUE),
    max=max(conc.ug.L, na.rm=TRUE)
  ) %>% 
  unique() %>% 
  mutate(metal_eng=case_when(
    metal=="As"~"Arsenic",
    metal=="Sb"~"Antimony",
    metal=="Ba"~"Barium",
    metal=="Be"~"Beryllium",
    metal=="Cs"~"Cesium",
    metal=="Co"~"Cobalt",
    metal=="Cd"~"Cadmium",
    metal=="Pb"~"Lead",
    metal=="Mn"~"Manganese",
    metal=="Hg"~"Mercury",
    metal=="Mo"~"Molybdenum",
    metal=="Tl"~"Thallium",
    metal=="Sn"~"Tin",
    metal=="W"~"Tungsten",
    metal=="U"~"Uranium",
    TRUE~as.character(metal))) %>% 
  add_column(source="NHANES")

Asnhanes.sumstats<-As.nhanes %>% 
  group_by(As_form) %>% 
  summarise(    min_osm=min(conc.osm, na.rm = TRUE),
                mean_osm=mean(conc.osm, na.rm=TRUE),
                median_osm=median(conc.osm,na.rm=TRUE),
                #GM = exp(mean(log(conc.osm))), 
                P95_osm=quantile(conc.osm, p=0.95, na.rm=TRUE),
                max_osm=max(conc.osm, na.rm=TRUE),
                min=min(conc, na.rm = TRUE),
                mean=mean(conc, na.rm=TRUE),
                median=median(conc,na.rm=TRUE),
                #GM = exp(mean(log(conc.ug.L))), 
                P95=quantile(conc, p=0.95, na.rm=TRUE),
                max=max(conc, na.rm=TRUE)) %>% 
  add_column(source="NHANES")

#### now combine manos sumstats and nhanes sumstats for graphing ####
manos.nhanes.sumstats<-rbind(metals.sumstats %>% mutate(source="MANOS"),nhanes.sumstats)
manos.nhanes.sumstatsAs<-rbind(sumstats_As %>% 
                                 filter(As_form=="AsFront"|As_form=="DMA"|As_form=="MA") %>% 
                                 filter(round!=5) %>% mutate(source="MANOS")
                               ,Asnhanes.sumstats)

#now combine manos and nhanes data (not sumstats) for graphing
manos.nhanes<-rbind(allmetals.nhanes %>% select(metal,conc.osm,source),
                    metalsR1R6_q %>% filter(round==1) %>% select(metal,Osmadjconc1) %>% 
                      rename(conc.osm=Osmadjconc1) %>% mutate(source="MANOS"))

#add inorganic As (sum of DMA and MA) to AsR1R6 df
#note this compares inorganic in MANOS to DMA and MA in NHANES
AsR1R6.1<-rbind(AsR1R6_q %>% select(MANOS_ID,round,As_form,conc1,Osmadjconc1),
                As_IOR1_q %>%  select(MANOS_ID,round,species,As,As_osmadj) %>% 
                  filter(species=="Inorganic") %>% 
                  rename(As_form=species,conc1=As,Osmadjconc1=As_osmadj) %>% 
                  mutate(As_form=ifelse(As_form=="Inorganic","AsI",As_form)))

manos.nhanes.As<-rbind(AsR1R6.1 %>% filter(As_form=="AsFront"|As_form=="DMA"|As_form=="MA"|As_form=="AsI") %>% 
                         filter(round!=5) %>% select(As_form,Osmadjconc1) %>% 
                         rename(conc.osm=Osmadjconc1) %>% mutate(source="MANOS"),
                       As.nhanes %>% select(As_form,conc.osm,source))

#make graph with osm-normalized concs 
#make side-by-side bar chart with MANOS data
#because scales are so different, making p1 and p2 (with different scales)
p1<-
  #ggplot(dat=manos.nhanes.sumstats %>% filter(metal_eng!="Arsenic"&metal_eng!="Molybdenum"&metal_eng!="Cesium"&metal_eng!="Beryllium"&metal_eng!="Uranium"&metal_eng!="Barium"))+
  ggplot(dat=manos.nhanes %>% filter(metal!="As"&metal!="Mo"&metal!="Cs"&metal!="Ba"&metal!="Be"&metal!="U"&metal!="Sb"&metal!="Sn"),
         aes(x=metal,y=conc.osm,fill=source))+
  #geom_bar(aes(x=metal_eng,y=conc.median_osm,fill=source),position="dodge",stat="identity")+
  geom_boxplot(outlier.shape=NA)+
  #stat_compare_means(aes(group=source),label = "p.signif",method="wilcox.test")+
  stat_compare_means(aes(group=source),label = "p.signif",method="wilcox.test")+
  scale_fill_manual(values=c("#3182bd", "#bdd7e7"))+ #"#0B406C","#add8e6"
  scale_y_continuous(limits=c(0,1.2), breaks = scales::pretty_breaks(n = 6))+
  theme_minimal()+
  theme(#aspect.ratio=1.8/1,
    legend.position = "none",
    #legend.position = "bottom",
    legend.title = element_blank(),
    #legend.text=element_text(size=22),
    legend.justification = c(0.9,0),
    axis.title.y = element_text(size=20,color="black"),
    axis.text.y = element_text(size=22,color = "black"),
    axis.text.x= element_text(angle = 45, vjust = 1, hjust=1,size=22,color="black"),
    axis.title.x=element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())+
  ylab("concentration (\u00b5g/L)")

p2<-
  #ggplot(dat=manos.nhanes.sumstats %>% filter(metal_eng=="Arsenic"|metal_eng=="Molybdenum"|metal_eng=="Cesium"|metal_eng=="Barium"))+
  ggplot(dat=manos.nhanes %>% filter(metal=="As"|metal=="Mo"|metal=="Cs"|metal=="Ba") %>% 
           mutate(metal=ifelse(metal=="As","UTAS",metal)),
         aes(x=metal,y=conc.osm,fill=source))+
  #geom_bar(aes(x=metal_eng,y=median_osm,fill=source),position="dodge",stat="identity")+
  geom_boxplot(outlier.shape=NA)+
  stat_compare_means(aes(group=source),label = "p.signif",method="wilcox.test")+
  #stat_compare_means(aes(group=source),label = "p.signif",method="wilcox.test")+
  scale_fill_manual(values=c("#3182bd", "#bdd7e7"))+ #"#0B406C","#add8e6"
  scale_y_continuous(limits=c(0,100),breaks = scales::pretty_breaks(n = 9))+
  theme_minimal()+
  theme(#aspect.ratio=1.25/1,
    legend.position = "none",
    axis.title.y = element_text(size=20,color="black"),
    axis.text.y = element_text(size=22,color="black"),
    axis.text.x= element_text(angle = 45, vjust = 1, hjust=1,size=22,color="black"),
    axis.title.x=element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())+
  ylab("concentration (\u00b5g/L)")

#make graph with As osm-normalized concs
p3<-
  #ggplot(dat=manos.nhanes.sumstatsAs)+
  ggplot(dat=manos.nhanes.As %>% 
           mutate(As_form=(ifelse(As_form=="AsFront","AB",As_form))),
         aes(x=As_form,y=conc.osm,fill=source))+
  #geom_bar(aes(x=As_form,y=median_osm,fill=source),position="dodge",stat="identity")+
  geom_boxplot(outlier.shape=NA)+
  scale_x_discrete(labels=c("AB",expression(Sigma*"As"),"DMA","MA"))+
  stat_compare_means(aes(group=source),label = "p.signif",method="wilcox.test")+
  #p.signif cuttoffs https://www.rdocumentation.org/packages/ggpubr/versions/0.6.0/topics/stat_compare_means
  scale_fill_manual(values=c("#3182bd", "#bdd7e7"))+ #"#0B406C","#add8e6"
  scale_y_continuous(limits=c(0,25),breaks = scales::pretty_breaks(n = 6))+
  theme_minimal()+
  theme(#aspect.ratio=1.8/1,
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text=element_text(size=22),
    legend.justification = c(0.9,0),
    axis.title.y = element_text(size=20,color="black"),
    axis.text.y = element_text(size=22,color = "black"),
    axis.text.x= element_text(angle = 45, vjust = 1, hjust=1,size=22,color="black"),
    axis.title.x=element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())+
  ylab("concentration (\u00b5g/L)")

p1/(p2+p3)
ggsave("..//results/NHANES_compare.jpg", width = 12, height = 9, units = "in")

#are the same people high in W and Mn?
#Max W is S-OF-249-13D-MA3-2018
#Max Mn is S-OF-361-13D-CO4-2018, S-OF-220-13D-MA3-2018, S-OF-277-13D-MA3-2018
#Max As
#Max Co
#Max Mo

####  compare MANOS to Nicaraguan and Navajo metal concs in Scammell 2020 ####

metalscradj.sumstats<-metalsR1_ucr %>% 
  select(MANOS_ID,metal,conc1,DFlag,UCradjconc1) %>% 
  group_by(metal) %>% 
  summarise(
    N=length(unique(MANOS_ID)),
    DF=signif(length(unique(MANOS_ID[DFlag==1]))/N*100), 
    min_cr=min(UCradjconc1, na.rm = TRUE),
    mean_cr=mean(UCradjconc1, na.rm=TRUE),
    median_ug.g=median(UCradjconc1,na.rm=TRUE),
    P95_cr=quantile(UCradjconc1, p=0.95, na.rm=TRUE),
    max_cr=max(UCradjconc1, na.rm=TRUE)) %>% 
  mutate(pop="MANOS",
         metal=ifelse(metal=="As","UTAS",metal))

R6metalscradj.sumstats<-metalsR6_ucr %>% 
  select(MANOS_ID,metal,conc1,DFlag,UCradjconc1) %>% 
  group_by(metal) %>% 
  summarise(
    N=length(unique(MANOS_ID)),
    DF=signif(length(unique(MANOS_ID[DFlag==1]))/N*100), 
    min_cr=min(UCradjconc1, na.rm = TRUE),
    mean_cr=mean(UCradjconc1, na.rm=TRUE),
    median_ug.g=median(UCradjconc1,na.rm=TRUE),
    P95_cr=quantile(UCradjconc1, p=0.95, na.rm=TRUE),
    max_cr=max(UCradjconc1, na.rm=TRUE)) %>% 
  mutate(pop="MANOS",
         metal=ifelse(metal=="As","UTAS",metal))

Ascradj.sumstats<-AsR1_ucr %>% 
  group_by(As_form) %>% 
  summarise(
    N=length(unique(MANOS_ID)),
    DF=signif(length(unique(MANOS_ID[DFlag==1]))/N*100), 
    min_cr=min(UCradjconc1, na.rm = TRUE),
    mean_cr=mean(UCradjconc1, na.rm=TRUE),
    median_ug.g=median(UCradjconc1,na.rm=TRUE),
    P95_cr=quantile(UCradjconc1, p=0.95, na.rm=TRUE),
    max_cr=max(UCradjconc1, na.rm=TRUE)) %>% 
  rename(metal=As_form) %>% 
  mutate(pop="MANOS",
         metal=ifelse(metal=="AsFront","Arsenobetaine",
                      ifelse(metal=="MA","MMA",metal)))

# read in Scammell 2020 data
Scam2020<-read_xlsx("C:/Users/krodger/Documents/BUSPH/MANOS/Data analysis/Data/processed/Metalcomparisons.xlsx")

compare<-bind_rows(Scam2020,metalscradj.sumstats,Ascradj.sumstats) %>% 
  filter(!grepl("_",metal),
         !grepl("(V)",metal),
         !grepl("AsTotal",metal))

ggplot(data=compare,aes(x=metal,y=median_ug.g,fill=pop))+
  geom_bar(stat="identity",position="dodge")

ggplot(dat=manos.nhanes.As,aes(x=As_form,y=conc.osm,fill=source))+
  #geom_bar(aes(x=As_form,y=median_osm,fill=source),position="dodge",stat="identity")+
  geom_boxplot(outlier.shape=NA)






