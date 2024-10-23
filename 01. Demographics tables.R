# author: KR
# date: 10/23/24
# purpose: make metals and speciated As datasets ready to work with

library(tidyverse)
library(openxlsx)

#read in metals and speciated As data
metalsR1R6_q<- read.csv("../results/metalsR1R6_q.csv")
As_IOR1R6_q<- read.csv("../results/As_IOR1R6_q.csv")

# make demographics table for metals, Table 2 in paper
table2<-metalsR1R6_q %>% 
  group_by(round,country) %>% 
  summarise(
    N=length(unique(MANOS_ID)),
    #country
    nic=length(unique(MANOS_ID[country=="Nicaragua"])),
    es=length(unique(MANOS_ID[country=="El Salvador"])),
    #age
    agecat1=length(unique(MANOS_ID[Age<25])),
    agecat2=length(unique(MANOS_ID[Age>24&Age<35])),
    agecat3=length(unique(MANOS_ID[Age>34])),
    agecatNA=length(unique(is.na(Age))),
    #smoking
    nSismoke=length(unique(MANOS_ID[Smokecurrent1=="Si"])), 
    nNosmoke=length(unique(MANOS_ID[Smokecurrent1=="No"])),
    smokeNA=length(unique(MANOS_ID[is.na(Smokecurrent)])),
    #seafood consumption
    nSeafoody=length(unique(MANOS_ID[SeafoodAnyYN==1])),
    nSeafoodn=length(unique(MANOS_ID[SeafoodAnyYN==0])),
    nSeafoodna=length(unique(MANOS_ID[is.na(SeafoodAnyYN)])),
    #agrochemical exposure
    nPestY=length(unique(MANOS_ID[PestYN==1])),
    nPestN=length(unique(MANOS_ID[PestYN==0])),
    nPestna=length(unique(MANOS_ID[is.na(PestYN)])),
    #drinking water habits (drank at day of interview visit 1 only)
    meanDWwork=mean(Totalwaterwork,na.rm=TRUE),
    sdDWwork=sd(Totalwaterwork,na.rm=TRUE),
    nDWwork=length(unique(MANOS_ID[!is.na(Totalwaterwork)])),
    meannotDWwork=mean(Totalnotwaterwork,na.rm=TRUE),
    sdnotDWwork=sd(Totalnotwaterwork,na.rm=TRUE),
    nnotDWwork=length(unique(MANOS_ID[!is.na(Totalnotwaterwork)])),
    #typical water consumed at work (visits 1 and 6)
    Ty_meanDWwork=mean(Typical_totalwaterwork,na.rm=TRUE),
    Ty_sdDWwork=sd(Typical_totalwaterwork,na.rm=TRUE),
    Ty_nDWwork=length(unique(MANOS_ID[!is.na(Typical_totalwaterwork)])),
    Ty_meannotDWwork=mean(Typical_totalnotwaterwork,na.rm=TRUE),
    Ty_sdnotDWwork=sd(Typical_totalnotwaterwork,na.rm=TRUE),
    Ty_nnotDWwork=length(unique(MANOS_ID[!is.na(Typical_totalnotwaterwork)])),
    #home drinking water source
    nPW=length(unique(MANOS_ID[DWhome3=="DWhomeprivatewell"])),
    nCW=length(unique(MANOS_ID[DWhome3=="DWhomecommonwell"])),
    nM=length(unique(MANOS_ID[DWhome3=="DWhomemuni"])),
    nMC=length(unique(MANOS_ID[DWhome3=="DWhomemunicommon"])),
    nO=length(unique(MANOS_ID[DWhome3=="DWhomeother"])),
    nR=length(unique(MANOS_ID[DWhome3=="DWhomeriver"])),
    DWhomeNA=length(unique(is.na(DWhome3))),
    #kidney function
    npoorkidfunc=length(unique(MANOS_ID[kidfunc==0])),
    nokkidfunc=length(unique(MANOS_ID[kidfunc==1])),
    goodkidfunc=length(unique(MANOS_ID[kidfunc==2])),
    #industry
    nsugar=length(unique(MANOS_ID[industry=="sugarcane"])),
    nbrick=length(unique(MANOS_ID[industry=="brick"])),
    ncorn=length(unique(MANOS_ID[industry=="corn"])),
    nplantain=length(unique(MANOS_ID[industry=="plantain"])),
    nroadcons=length(unique(MANOS_ID[industry=="construction"])),
    indother=length(unique(MANOS_ID[industry=="other"])),
    indNA=length(unique(MANOS_ID[is.na(industry)])),
    #worksite
    nAZ1=length(unique(MANOS_ID[worksite=="AZ1"])),
    nAZ2=length(unique(MANOS_ID[worksite=="AZ2"])),
    nAZ5=length(unique(MANOS_ID[worksite=="AZ5"])),
    nAZ7=length(unique(MANOS_ID[worksite=="AZ7"])),
    nCO4=length(unique(MANOS_ID[worksite=="CO4"])),
    nLA8=length(unique(MANOS_ID[worksite=="LA8"])),
    nMA3=length(unique(MANOS_ID[worksite=="MA3"])),
    nPL1=length(unique(MANOS_ID[worksite=="PL1"])),
    nPL9=length(unique(MANOS_ID[worksite=="PL9"])),
    nOtherwork=length(unique(MANOS_ID[worksite=="Other"])),
    #residential department
    nresA=length(unique(MANOS_ID[Dept4=="Ahuachapan"])),
    nresCa=length(unique(MANOS_ID[Dept4=="Cabanas"])),
    nresCh=length(unique(MANOS_ID[Dept4=="Chinandega"])),
    nresCu=length(unique(MANOS_ID[Dept4=="Cuscatlan"])),
    nresLL=length(unique(MANOS_ID[Dept4=="LaLibertad"])),
    nresLe=length(unique(MANOS_ID[Dept4=="Leon"])),
    nresSS=length(unique(MANOS_ID[Dept4=="SanSalvador"])),
    nresSA=length(unique(MANOS_ID[Dept4=="SantaAna"])),
    nresSo=length(unique(MANOS_ID[Dept4=="Sonsonate"])),
    nresU=length(unique(MANOS_ID[Dept4=="Usulutan"])),
    nresother=length(unique(MANOS_ID[Dept4=="Other"])),
    resNA=length(unique(MANOS_ID[is.na(Dept4)])),
    #count participants who work and live in same department
    nsame=length(unique(MANOS_ID[samedept=="same"])),
    ndiff=length(unique(MANOS_ID[samedept=="diff"])), #not right
    nother=length(unique(MANOS_ID[samedept=="other"])),
    nNAsamediff=length(unique(MANOS_ID[is.na(samedept)])),
  ) %>% t %>% as.data.frame()
#write.csv(table2,"..//results/Table2.csv")

#make table S3 speciated As demographics
Astable1<-As_IOR1R6_q %>% 
  filter(round!=5) %>% 
  group_by(round,country) %>% 
  summarise(
    N=length(unique(MANOS_ID)),
    #country
    nic=length(unique(MANOS_ID[country=="Nicaragua"])),
    es=length(unique(MANOS_ID[country=="El Salvador"])),
    #age
    agecat1=length(unique(MANOS_ID[Age<25])),
    agecat2=length(unique(MANOS_ID[Age>24&Age<35])),
    agecat3=length(unique(MANOS_ID[Age>34])),
    agecatNA=length(unique(is.na(Age))),
    #smoking
    nSismoke=length(unique(MANOS_ID[Smokecurrent1=="Si"])), 
    nNosmoke=length(unique(MANOS_ID[Smokecurrent1=="No"])),
    smokeNA=length(unique(MANOS_ID[is.na(Smokecurrent)])),
    #seafood
    nSeafoody=length(unique(MANOS_ID[SeafoodAnyYN==1])),
    nSeafoodn=length(unique(MANOS_ID[SeafoodAnyYN==0])),
    nSeafoodna=length(unique(MANOS_ID[is.na(SeafoodAnyYN)])),
    #agrochemical exposure
    nPestY=length(unique(MANOS_ID[PestYN==1])),
    nPestN=length(unique(MANOS_ID[PestYN==0])),
    nPestna=length(unique(MANOS_ID[is.na(PestYN)])),
    #water
    #drinking water habits (drank at day of interview visit 1 only)
    meanDWwork=mean(Totalwaterwork,na.rm=TRUE),
    sdDWwork=sd(Totalwaterwork,na.rm=TRUE),
    nDWwork=length(unique(MANOS_ID[!is.na(Totalwaterwork)])),
    meannotDWwork=mean(Totalnotwaterwork,na.rm=TRUE),
    sdnotDWwork=sd(Totalnotwaterwork,na.rm=TRUE),
    nnotDWwork=length(unique(MANOS_ID[!is.na(Totalnotwaterwork)])),
    #typical water consumed at work (visits 1 and 6)
    Ty_meanDWwork=mean(Typical_totalwaterwork,na.rm=TRUE),
    Ty_sdDWwork=sd(Typical_totalwaterwork,na.rm=TRUE),
    Ty_nDWwork=length(unique(MANOS_ID[!is.na(Typical_totalwaterwork)])),
    Ty_meannotDWwork=mean(Typical_totalnotwaterwork,na.rm=TRUE),
    Ty_sdnotDWwork=sd(Typical_totalnotwaterwork,na.rm=TRUE),
    Ty_nnotDWwork=length(unique(MANOS_ID[!is.na(Typical_totalnotwaterwork)])),
    # home drinking water source
    nPW=length(unique(MANOS_ID[DWhome3=="DWhomeprivatewell"])),
    nCW=length(unique(MANOS_ID[DWhome3=="DWhomecommonwell"])),
    nM=length(unique(MANOS_ID[DWhome3=="DWhomemuni"])),
    nMC=length(unique(MANOS_ID[DWhome3=="DWhomemunicommon"])),
    nO=length(unique(MANOS_ID[DWhome3=="DWhomeother"])),
    nR=length(unique(MANOS_ID[DWhome3=="DWhomeriver"])),
    DWhomeNA=length(unique(is.na(DWhome3))),
    # kidney function
    npoorkidfunc=length(unique(MANOS_ID[kidfunc==0])),
    nokkidfunc=length(unique(MANOS_ID[kidfunc==1])),
    goodkidfunc=length(unique(MANOS_ID[kidfunc==2])),
    # industry
    nsugar=length(unique(MANOS_ID[industry=="sugarcane"])),
    nbrick=length(unique(MANOS_ID[industry=="brick"])),
    ncorn=length(unique(MANOS_ID[industry=="corn"])),
    nplantain=length(unique(MANOS_ID[industry=="plantain"])),
    nroadcons=length(unique(MANOS_ID[industry=="construction"])),
    nindother=length(unique(MANOS_ID[industry=="other"])),
    indNA=length(unique(MANOS_ID[is.na(industry)])),
    #worksite
    nAZ1=length(unique(MANOS_ID[worksite=="AZ1"])),
    nAZ2=length(unique(MANOS_ID[worksite=="AZ2"])),
    nAZ5=length(unique(MANOS_ID[worksite=="AZ5"])),
    nAZ7=length(unique(MANOS_ID[worksite=="AZ7"])),
    nCO4=length(unique(MANOS_ID[worksite=="CO4"])),
    nLA8=length(unique(MANOS_ID[worksite=="LA8"])),
    nMA3=length(unique(MANOS_ID[worksite=="MA3"])),
    nPL1=length(unique(MANOS_ID[worksite=="PL1"])),
    nPL9=length(unique(MANOS_ID[worksite=="PL9"])),
    nOtherwork=length(unique(MANOS_ID[worksite=="Other"])),
    #residential department
    nresA=length(unique(MANOS_ID[Dept4=="Ahuachapan"])),
    nresCa=length(unique(MANOS_ID[Dept4=="Cabanas"])),
    nresCh=length(unique(MANOS_ID[Dept4=="Chinandega"])),
    nresCu=length(unique(MANOS_ID[Dept4=="Cuscatlan"])),
    nresLL=length(unique(MANOS_ID[Dept4=="LaLibertad"])),
    nresLe=length(unique(MANOS_ID[Dept4=="Leon"])),
    nresSS=length(unique(MANOS_ID[Dept4=="SanSalvador"])),
    nresSA=length(unique(MANOS_ID[Dept4=="SantaAna"])),
    nresSo=length(unique(MANOS_ID[Dept4=="Sonsonate"])),
    nresU=length(unique(MANOS_ID[Dept4=="Usulutan"])),
    nresother=length(unique(MANOS_ID[Dept4=="Other"])),
    resNA=length(unique(MANOS_ID[is.na(Dept4)])),
    # live and work in same department
    nsame=length(unique(MANOS_ID[samedept=="same"])),
    ndiff=length(unique(MANOS_ID[samedept=="diff"])),
    nother=length(unique(MANOS_ID[samedept=="other"])),
    nNAsamediff=length(unique(MANOS_ID[is.na(samedept)])),
  ) %>% t %>% as.data.frame()
#write.csv(Astable1,"..//results/AsTable1.csv")

test<-As_IOR1R6_q %>% 
  select(MANOS_ID,round,country,samedept) %>% 
  filter(round==6) %>% unique() %>% 
  summarise(
    nsame=length(unique(MANOS_ID[samedept=="same"])),
    ndiff=length(unique(MANOS_ID[samedept=="diff"])),
    nother=length(unique(MANOS_ID[samedept=="other"])),
    nNAsamediff=length(unique(MANOS_ID[is.na(samedept)])))


