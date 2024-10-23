# author: KR
# date: 10/23/24
# purpose: make metals and speciated As datasets ready to work with

library(tidyverse)
library(openxlsx)

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
#write.csv(metals.sumstats,"..//results/metals.sumstats.csv")

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

#sum stats for inorganic/organic arsenic
sumstats_IO<-As_IOR1R6_q %>% 
  group_by(species,round,country) %>% 
  summarise(
    N=length(unique(MANOS_ID)),
    #D=sum(DFlag==1),
    #DF=D/N*100, #DF=100% from sumstats_As
    min=signif(min(As, na.rm = TRUE),4),
    mean=signif(mean(As,na.rm=TRUE),4),
    osm_mean=signif(mean(As_osmadj,na.rm=TRUE),4),
    SD=signif(sd(As,na.rm=TRUE),4),
    osm_SD=signif(sd(As_osmadj,na.rm=TRUE),4),
    median=signif(median(As,na.rm=TRUE),4),
    osm_median=signif(median(As_osmadj,na.rm=TRUE),4),
    #GM = signif(exp(mean(log(conc1_ASspecies))),2), 
    P95=signif(quantile(As, p=0.95, na.rm=TRUE),4),
    osm_P95=signif(quantile(As_osmadj, p=0.95, na.rm=TRUE),4),
    max=signif(max(As, na.rm=TRUE),4),
    meanMDL=mean(As_MDL,na.rm=TRUE)
  ) %>% 
  unique()
#write.csv(sumstats_IO,"..//results/As_IO.sumstats.csv")


